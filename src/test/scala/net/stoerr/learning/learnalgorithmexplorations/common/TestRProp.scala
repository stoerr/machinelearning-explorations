package net.stoerr.learning.learnalgorithmexplorations.common

import DValue._
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.12.2014
 */
class TestRProp extends AnyFunSuite {

  val eps = 1e-7

  test("rprop") {
    def fd(args: Array[DValue]) = {
      // minimum (0,0)
      val dif = args(0) - args(1)
      dif.cosh + (args(0) * args(0) + args(1) * args(1))
    }
    val f: (Array[Double]) => Double = asDoubleFunction(fd)
    val fgrad: (Array[Double]) => (Double, Array[Double]) = asDoubleFunctionWithGradient(fd)
    val (x, y, dy) = new RProp(f, fgrad, 100, Array(1001.0, 1000.0)).descent()
    println(x.toList + "\t" + y + "\t" + dy)
  }

}
