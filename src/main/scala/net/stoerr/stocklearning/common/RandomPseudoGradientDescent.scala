package net.stoerr.stocklearning.common

import java.lang.Math._

import net.stoerr.stocklearning.common.DoubleArrayVector._

/** We try to approximate gradient descent without calculating the gradient: in each step, the previous direction
  * is corrected by a random vector, and then a step in the resulting maximum slope direction is quadratically
  * approximated. */
case class RandomPseudoGradientDescent(f: Array[Double] => Double, dim: Int, var x: Array[Double]) {

  def this(f: Array[Double] => Double, dim: Int) = this(f, dim, randomVector(dim))

  var laststep: Array[Double] = randomVector(dim).normalize * eps

  def step() = {
    val y = f(x)
    println()
    println("y: " + y)

    val yld = f(laststep * 0.25 + x)
    var random = randomVector(dim).normalize
    random = (random - laststep.normalize * (laststep.normalize * random)).normalize
    assert(abs(random * laststep.normalize) < eps)
    random = random * (laststep.abs)
    val yr = f(random * 0.25 + x)

    var newdirection = (laststep * (y - yld) + random * (y - yr)).normalize * laststep.abs * 0.25
    println("angle: " + (newdirection.normalize * laststep.normalize))

    val y1 = f(x + newdirection)
    val y2 = f(x + newdirection * 2)
    var factor = GradientDescent.interpolatedMinimum(0, y, 1, y1, 2, y2)
    println("factor: " + factor)
    factor = if (factor > 16) 16 else if (factor < -8) -8 else factor

    laststep = newdirection * factor
    x = x + laststep
  }

}
