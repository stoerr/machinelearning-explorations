package net.stoerr.learning.learnalgorithmexplorations.common

import DoubleArrayVector._

/** We try to approximate gradient descent without calculating the gradient: in each step, the previous direction
  * is corrected by a random vector, and then a step in the resulting maximum slope direction is quadratically
  * approximated. */
case class RandomPseudoGradientDescent(f: Vec => Double, dim: Int, var x: Vec) {

  def this(f: Vec => Double, dim: Int) = this(f, dim, randomVector(dim))

  var laststep: Vec = randomVector(dim).normalize * eps
  var numstep = 0

  @Deprecated
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

    val newdirection = (laststep * (yback - yforw) + random * (yleft - yright)).normalize * laststep.abs

    val y1 = f(x + newdirection * 0.5)
    val y2 = f(x + newdirection)
    val (factor, yn) = NumericalMinimumFinder.singleStep(xp => f(x + newdirection * xp), (0, y), (0.5, y1), (1, y2))

    /* for (t <- 0 until 5) {
      val xx = t * 0.5 - 1
      println(s"r(${xx})=${f(x + random * xx) - y}")
    }
    for (t <- 0 until 5) {
      val xx = t * 0.5
      println(s"f(${xx})=${f(x + newdirection * xx) - y}")
    } */

    // println(s"angle:${newdirection.normalize * laststep.normalize} angler:${newdirection.normalize * random.normalize} factor: ${factor} step: ${newdirection.abs * factor}")
    laststep = newdirection * factor
    x = x + laststep
  }

  def stepOrthogonalRandom(): Double = {
    numstep = numstep + 1
    val newdirection = laststep.randomOrthogonalVector()

    def fdir(xs: Double): Vec = x + newdirection * xs

    def fstep(xs: Double): Double = f(fdir(xs))

    val (xn, yn) = NumericalMinimumFinder.secondorderMinimumSearch(fstep)
    println(s"y: $yn, xs: $xn ($numstep) ${laststep.abs}")
    if (xn == 0) {
      println("[" + x.mkString(",") + "]")
      println("[" + laststep.mkString(",") + "]")
      println("[" + newdirection.mkString(",") + "]")
    }
    laststep = fdir(xn) - x
    x = fdir(xn)
    yn
  }

}
