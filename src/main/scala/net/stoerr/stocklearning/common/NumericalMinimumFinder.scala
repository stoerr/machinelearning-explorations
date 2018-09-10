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
  protected def interpolatedMinimumStep(xy0: (Double, Double), xy1: (Double, Double), xy2: (Double, Double), maxstep: Double = 4): Double = {
    val Array((x0, y0), (x1, y1), (x2, y2)) = Array(xy0, xy1, xy2).sortBy(_._1)
    val xm = (x0 + x1 + x2) / 3
    val xd = (x2 - x0)
    var xmin = interpolatedMinimum(x0, y0, x1, y1, x2, y2)
    if (abs(xmin - xm) < maxstep * xd) xmin else xm + signum(xmin - xm) * xd * maxstep

  }

  /** Search for minimum in several steps; we start with x=0,1,2 */
  def secondorderMinimumSearch(f: Double => Double, maxstep: Double = 4, maxiter: Int = 5, eps: Double = 1e-6): (Double, Double) = {
    var Array(xy0, xy1, xy2) = Array((0.0, f(0.0)), (1.0, f(1.0)), (2.0, f(2.0)))
    for (_ <- 0 until maxiter if (xy2._1 - xy0._1 > eps)) {
      val xmin = interpolatedMinimumStep(xy0, xy1, xy2, maxstep)
      val ymin = f(xmin)
      val newxy = Array(xy0, xy1, xy2, (xmin, ymin)).sortBy(xy => abs(xy._1 - xmin)).take(3).sortBy(_._1)
      xy0 = newxy(0)
      xy1 = newxy(1)
      xy2 = newxy(2)
      // println(List(xy0, xy1, xy2))
    }
    Array(xy0, xy1, xy2).sortBy(_._2).apply(0)
  }

}
