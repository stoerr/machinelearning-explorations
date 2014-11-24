package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector._

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
    if (x0 == x1 && x1 == x2) x1
    else (x0 * x0 * (y1 - y2) + x1 * x1 * (y2 - y0) + x2 * x2 * (y0 - y1)) / 2 /
      (x0 * y1 - x0 * y2 - x1 * y0 + x1 * y2 + x2 * y0 - x2 * y1)

  /** Numerically finds a rough approximation of the minimum of f near 0 with first step length eps. <br/>
    * Optimized for what we need in gradient descent: We assume the sign of eps is into the direction of the minimum
    * and f goes downward into that direction. If this doesn't nold, the algorithm might not terminate.
    * f0 is f(0); given to save calculations of f
    */
  def approximateMinimum(f0: Double, f: Double => Double, eps: Double): Double = {
    var (x0, y0) = (0.0, f0)
    var (x1, y1) = (eps, f(eps))
    if (math.abs(y0 - y1) < 1e-7 || y1.isInfinite || y1.isNaN) return 0
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
    // println(((x0, x1, x2), (y0, y1, y2)))
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
        assert(math.abs(x0 - x2) > 1e-20)
      }
      // println(((x0, x1, x2), (y0, y1, y2)))
      if (math.abs(x2) > 1e6 || math.abs(x2) < 1e-6 || y2.isInfinite || y2.isNaN) return 0
    }
    val min = interpolatedMinimum(x0, y0, x1, y1, x2, y2)
    assert((min >= x0) && (min <= x2) || (min <= x0) && (min >= x2), (x0, y0) + "\t" +(x1, y1) + "\t" +(x2, y2) + "\t" + min)
    min
  }

}

abstract class AbstractGradientDescent(val f: Array[Double] => Double, val fgrad: Array[Double] => (Double, Array[Double]),
                                       val maxSteps: Int, x0: Array[Double], eps0: Double = -1.0) {
  // println("-- descent")
  var lastY = Double.MaxValue
  var eps = eps0
  var x = x0
  var ygrad: (Double, Array[Double]) = fgrad(x)

  def descent(): (Array[Double], Double, Double) = {
    for (i <- 0 until maxSteps if math.abs(eps) > 1e-8) {
      val (y, grad) = ygrad
      eps = calculateStep(y, grad)
      // println(i + "\t" + y + "\t" + eps)
      x = x + grad * eps
      ygrad = fgrad(x)
      if (math.abs(lastY - y) < 1e-8) eps = 0 // stop.
      lastY = y
    }
    val y = f(x)
    (x, y, math.abs(y - lastY))
  }

  protected def calculateStep(y: Double, grad: Array[Double]): Double
}

class GradientDescentWithWithMinimumApproximation
(f: Array[Double] => Double, fgrad: Array[Double] => (Double, Array[Double]),
 maxSteps: Int, x0: Array[Double], eps0: Double = -1.0)
  extends AbstractGradientDescent(f, fgrad, maxSteps, x0, eps0) {

  override protected def calculateStep(y: Double, grad: Array[Double]): Double = {
    val directedFunc: (Double) => Double = x.directionalFunction(f, grad)
    GradientDescent.approximateMinimum(y, directedFunc, eps)
  }
}

/** We calculate another gradient somewhat into the direction of the gradient, and calculate the step width by
  * estimating the second derivation. */
class GradientDescentPseudoLinearNewton
(f: Array[Double] => Double, fgrad: Array[Double] => (Double, Array[Double]),
 maxSteps: Int, x0: Array[Double], eps0: Double = -1.0)
  extends AbstractGradientDescent(f, fgrad, maxSteps, x0, eps0) {

  override protected def calculateStep(y: Double, grad: Array[Double]): Double = {
    val x1 = x + grad * eps
    val (y1, grad1) = fgrad(x1)
    eps / (1 - (grad * grad1) / (grad * grad))
  }
}

/** We calculate another gradient somewhat into the direction of the gradient, and
  * then calculate the step that the resulting gradient should be minimal. */
class GradientDescentMinimizeGradient
(f: Array[Double] => Double, fgrad: Array[Double] => (Double, Array[Double]),
 maxSteps: Int, x0: Array[Double], eps0: Double = -1.0)
  extends AbstractGradientDescent(f, fgrad, maxSteps, x0, eps0) {

  override protected def calculateStep(y: Double, grad: Array[Double]): Double = {
    val x1 = x + grad * eps
    val (y1, grad1) = fgrad(x1)
    // eps * (grad * grad1 - grad * grad) / (grad * grad + grad1 * grad1 - grad * grad1)
    eps * (grad * grad - grad * grad1) / (grad * grad + grad1 * grad1 - grad * grad1 * 2)
  }
}
