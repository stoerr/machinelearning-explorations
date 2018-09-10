package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector._

/** We try to approximate gradient descent without calculating the gradient: in each step, the previous direction
  * is corrected by a random vector, and then a step in the resulting maximum slope direction is quadratically
  * approximated. */
case class RandomPseudoGradientDescent(f: Vec => Double, dim: Int, var x: Vec) {

  def this(f: Vec => Double, dim: Int) = this(f, dim, randomVector(dim))

  var laststep: Vec = randomVector(dim).normalize * eps
  var numstep = 0

  def step(): Unit = {
    numstep = numstep + 1
    val y = f(x)
    println()
    println(s"y: $y ($numstep)")

    val yld = f(laststep * 0.25 + x)
    val random = laststep.randomOrthogonalVector()
    val yr = f(random * 0.25 + x)

    val newdirection = (laststep * (y - yld) + random * (y - yr)).normalize * laststep.abs * 0.25
    println("angle: " + (newdirection.normalize * laststep.normalize))

    val y1 = f(x + newdirection)
    val y2 = f(x + newdirection * 2)
    var factor = NumericalMinimumFinder.interpolatedMinimum(0, y, 1, y1, 2, y2)
    println("factor: " + factor)
    factor = if (factor > 256) 256 else if (factor < -128) -128 else factor

    laststep = newdirection * factor
    x = x + laststep
  }

  def stepOrthogonalRandom(): Unit = {
    numstep = numstep + 1
    val newdirection = laststep.randomOrthogonalVector()

    def fdir(xs: Double): Vec = x + newdirection * xs

    def fstep(xs: Double): Double = f(fdir(xs))

    val (xn, yn) = NumericalMinimumFinder.secondorderMinimumSearch(fstep)
    println(s"y: $yn, xs: $xn ($numstep)")
    laststep = fdir(xn) - x
    x = fdir(xn)
  }

}
