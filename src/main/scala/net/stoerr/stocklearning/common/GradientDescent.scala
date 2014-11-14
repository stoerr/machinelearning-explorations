package net.stoerr.stocklearning.common

/**
 * Implements gradient descent
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.11.2014
 */
object GradientDescent {

  /** Find minimum by quadratical interpolation, solved by http://www.mathics.net/ <br/>
    * {{x->x02y1-x02y2-x12y0+x12y2+x22y0-x22y12(x0y1-x0y2-x1y0+x1y2+x2y0-x2y1)}}
    */
  def interpolatedMinimum(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double =
    (x0 * x0 * (y1 - y2) + x1 * x1 * (y2 - y0) + x2 * x2 * (y0 - y1)) / 2 / (x0 * y1 - x0 * y2 - x1 * y0 + x1 * y2 + x2 * y0 - x2 * y1)

  /** Numerically finds a rough approximation of the minimum of f near 0 with first step length eps. <br/>
    * We assume the sign of eps is into the direction of the minimum and f goes downward into that direction.
    * Optimized for what we need in gradient descent. f0 is f(0); given to save calculations of f
    */
  def approximateMinimum(f: Double => Double, eps: Double, f0: Double): Double = {
    var (x0, y0) = (0.0, f0)
    var (x1, y1) = (eps, f(eps))
    var (x2, y2) = (0.0, 0.0)
    if (y1 >= y0) {
      x2 = x1
      y2 = y1
      x1 = x2 / 2
      y1 = f(x1)
    } else {
      x2 = x1 * 2
      y2 = f(x2)
    }
    // println(((x0, x1, x2),(y0,y1,y2)))
    while (y1 > math.min(y0, y2)) {
      if (y2 < y0) {
        // y0 >= y1 >= y2
        x0 = x1
        y0 = y1
        x1 = x2
        y1 = y2
        x2 = x1 + 2 * (x1 - x0)
        y2 = f(x2)
      } else {
        // y0 <= y1 <= y2
        x2 = x1
        y2 = y1
        x1 = (x0 + x2) / 2
        y1 = f(x1)
      }
      // println(((x0, x1, x2),(y0,y1,y2)))
    }
    val min = interpolatedMinimum(x0, y0, x1, y1, x2, y2)
    assert(min > x0)
    assert(min < x2)
    min
  }

}
