package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DValue._
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.12.2014
 */
class TestRProp extends FunSuite {

  val eps = 1e-7

  test("rprop") {
    def fd(args: Array[DValue]) = {
      // minimum (0,0)
      val dif = args(0) - args(1)
      dif.cosh + (args(0) * args(0) + args(1) * args(1))
    }
    val f: (Array[Double]) => Double = asDoubleFunction(fd)
    val fgrad: (Array[Double]) => (Double, Array[Double]) = asDoubleFunctionWithGradient(fd)
    val rprop = new RProp(f, fgrad, 25, Array(2.0, 3.0))
    for (i <- 0 until 100) {
      rprop.step()
    }
  }

}
