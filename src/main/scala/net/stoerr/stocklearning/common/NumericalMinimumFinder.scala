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

}
