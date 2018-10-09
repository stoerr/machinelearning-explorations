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

    val yforw = f(laststep * 0.25 + x)
    val yback = f(laststep * -0.25 + x)
    val random = laststep.randomOrthogonalVector()
    val yright = f(random * 0.25 + x)
    val yleft = f(random * -0.25 + x)

    val newdirection = (laststep * (yforw - yback) + random * (yright - yleft)).normalize * laststep.abs * 0.25

    val y1 = f(x + newdirection)
    val y2 = f(x + newdirection * 2)
    val (factor, yn) = NumericalMinimumFinder.singleStep(xp => f(x + newdirection * xp), (0, y), (1, y1), (2, y2))
    laststep = newdirection * factor
    println(s"angle:${newdirection.normalize * laststep.normalize} factor: ${factor} step: ${laststep.abs}")
    x = x + laststep
  }

  def stepOrthogonalRandom(): Unit = {
    numstep = numstep + 1
    val newdirection = laststep.randomOrthogonalVector()

    def fdir(xs: Double): Vec = x + newdirection * xs

    def fstep(xs: Double): Double = f(fdir(xs))

    val (xn, yn) = NumericalMinimumFinder.secondorderMinimumSearch(fstep)
    println(s"y: $yn, xs: $xn ($numstep) ${laststep.abs}")
    laststep = fdir(xn) - x
    x = fdir(xn)
  }

}
