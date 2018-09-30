package net.stoerr.stocklearning.common

import Math._

object NumericalMinimumFinder {

  /** Find minimum by quadratical interpolation, solved by http://www.mathics.net/ <br/>
    * {{x->x02y1-x02y2-x12y0+x12y2+x22y0-x22y12(x0y1-x0y2-x1y0+x1y2+x2y0-x2y1)}}
    */
  def interpolatedMinimum(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double =
    if (x0 == x1 && x1 == x2) x1
    else (x0 * x0 * (y1 - y2) + x1 * x1 * (y2 - y0) + x2 * x2 * (y0 - y1)) / 2 /
      (x0 * y1 - x0 * y2 - x1 * y0 + x1 * y2 + x2 * y0 - x2 * y1)

  def derivatives(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): (Double, Double) = {
    def taylor(xa: Double, ya: Double, xb: Double, yb: Double) =
      ((xa * xa * yb - xb * xb * ya) / (xa * xb * (xa - xb)), 2 * (-xa * yb + xb * ya) / (xa * xb * (xa - xb)))

    taylor(x1 - x0, y1 - y0, x2 - x0, y2 - y0)
  }


  /** Limits #interpolatedMinimum to maxstep times the spread of the x values. */
  def interpolatedMinimumStep(f: Double => Double, xy0: (Double, Double), xy1: (Double, Double), xy2: (Double, Double), maxstep: Double = 4): (Double, Double) = {
    val Array((x0, y0), (x1, y1), (x2, y2)) = Array(xy0, xy1, xy2).sortBy(_._1)
    if (y1 - y0 < (x1 - x0) * (y2 - y0) / (x2 - x0)) {
      return addF(f, if (y0 < y2) x0 - 2 * (x2 - x0) else x2 + 2 * (x2 - x0))
    }
    val xm = (x0 + x1 + x2) / 3
    val xd = (x2 - x0)
    val rxmin = interpolatedMinimum(x0, y0, x1, y1, x2, y2)
    val xmin = if (abs(rxmin - xm) < maxstep * xd) rxmin else xm + signum(rxmin - xm) * xd * maxstep
    val ymin = f(xmin)
    (xmin, ymin)
  }

  protected def addF(f: Double => Double, x: Double): (Double, Double) = (x, f(x))

  /** When the 2nd derivation is negative, we need desperate measures. :-) */
  protected def hilldescent(f: Double => Double, rxy0: (Double, Double), rxy1: (Double, Double), rxy2: (Double, Double), maxstep: Double = 4): (Double, Double) = {
    val sortedparms = Array(rxy0, rxy1, rxy2).sortBy(_._1)
    val Array((x0, y0), (x1, y1), (x2, y2)) = if (sortedparms(0)._2 > sortedparms(2)._2) sortedparms else sortedparms.reverse
    val xn = x2 + maxstep * (x2 - x0)
    val yn = f(xn)
    if (yn < y2) (xn, yn) else {
      interpolatedMinimumStep(f, (x1, y1), (x2, y2), (xn, yn), 1.01)
    }
  }

  /** Search for minimum in several steps; we start with x=0,1,2 */
  def secondorderMinimumSearch(f: Double => Double, maxstep: Double = 16, maxiter: Int = 5, eps: Double = 1e-6): (Double, Double) = {
    var Array(xy0, xy1, xy2) = Array((0.0, f(0.0)), (1.0, f(1.0)), (2.0, f(2.0)))
    for (_ <- 0 until maxiter if (xy2._1 - xy0._1 > eps)) {
      val xymin = interpolatedMinimumStep(f, xy0, xy1, xy2, maxstep)
      val newxy = Array(xy0, xy1, xy2, xymin).sortBy(xy => abs(xy._1 - xymin._1)).take(3).sortBy(_._1)
      xy0 = newxy(0)
      xy1 = newxy(1)
      xy2 = newxy(2)
      // println(List(xy0, xy1, xy2))
    }
    Array(xy0, xy1, xy2).sortBy(_._2).apply(0)
  }

  /** Search for minimum in several steps; we start with x=0,1,2 */
  def secondorderMinimumSearch2(f: Double => Double, maxstep: Double = 16, maxiter: Int = 5, eps: Double = 1e-6): (Double, Double) = {
    val stepper = Stepper((0.0, f(0.0)), (1.0, f(1.0)), (2.0, f(2.0)), f)
    for (_ <- 0 until maxiter if (stepper.ymax - stepper.ymin > eps)) {
      stepper.step()
    }
    stepper.xy0
  }

}

/** We estimate the point of the minimum by either taking the 2nd order interpolation in case of valleys,
  * or stepping outside the interval in the case of hills. If we are outside the interval, and we actually went up the slope, we go a part of the last step, instead.
  * In the case of valleys, we want to use the 2nd order interpolation unless it results in a higher f. Otherwise use downhill, too.
  * xy0 is last location, xy1 after previous step, xy2 two steps before. */
case class Stepper(var xy0: (Double, Double), var xy1: (Double, Double), var xy2: (Double, Double), f: Double => Double) {
  def x0: Double = xy0._1

  def y0: Double = xy0._2

  def x1: Double = xy1._1

  def y1: Double = xy1._2

  def x2: Double = xy2._1

  def y2: Double = xy2._2

  def xmin: Double = Array(x0, x1, x2).min

  def xmax: Double = Array(x0, x1, x2).max

  def ymax: Double = Array(y0, y1, y2).max

  def ymin: Double = Array(y0, y1, y2).min

  override def toString: String = Array(xy0, xy1, xy2).mkString(" ")

  /** Approximation of first derivative at x0 */
  def d1: Double = NumericalMinimumFinder.derivatives(x0, y0, x1, y1, x2, y2)._1

  /** Approximation of second derivative at x0 */
  def d2: Double = NumericalMinimumFinder.derivatives(x0, y0, x1, y1, x2, y2)._2

  def valley: Boolean = d2 > 0

  private def outside(x: Double) = (x - x0) * (x2 - x) < 0

  private def insequence(xa: Double, xb: Double, xc: Double) = (xa - xb) * (xb - xc) > 0

  private val maxstep = Math.pow(Math.E, Math.PI)

  def step(): Unit = {
    val xyn = xynext()
    val Array(xya, xyb) = Array(xy0, xy1).sortBy(-_._2) // ya >= yb
    xy2 = xya
    xy1 = xyb
    xy0 = xyn
  }

  def xynext(): (Double, Double) = {
    println(this)
    if (valley) {
      var xest = NumericalMinimumFinder.interpolatedMinimum(x0, y0, x1, y1, x2, y2)
      xest = max(xest, xmin - maxstep * (xmax - xmin))
      xest = Math.min(xest, xmax + maxstep * (xmax - xmin))
      var yest = f(xest)
      if (yest < max(y0, y1)) {
        println("interpol -> " + xest)
        return (xest, yest)
      } else {
        println("interpol not - " + (xest, yest))
      }
    }
    val Array((xa, ya), (xb, yb)) = Array(xy0, xy1).sortBy(-_._2) // ya >= yb
    var xest = xb + (xb - xa) * (Math.E - 1)
    var yest = f(xest)
    if (yest < yb) {
      println("bigstep -> " + xest)
      return (xest, yest)
    }
    xest = xb + (xb - xa) / (Math.PI - 1)
    println("smallstep -> " + xest)
    (xest, f(xest))
  }

}
