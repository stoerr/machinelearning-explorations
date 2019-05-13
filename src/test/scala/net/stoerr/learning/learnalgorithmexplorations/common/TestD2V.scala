package net.stoerr.learning.learnalgorithmexplorations.common

import org.scalatest.FunSuite

/**
 * Tests for D2V
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 12.11.2014
 */
class TestD2V extends FunSuite {

  val eps = 1e-7

  def deriv(f: Double => Double, x: Double) = (f(x + eps) - f(x - eps)) / (2 * eps)

  val eps2 = 1e-5

  def d2(f: Double => Double, x: Double) = (f(x + eps2) + f(x - eps2) - 2 * f(x)) / (eps2 * eps2)

  test("torough") {
    checkFunction(x => (x - 4) / (x + 3), x => (x - D2V(4)) / (x + D2V(3)))
    checkFunction(x => x * x * x, x => x * x * x)
    checkFunction(x => x * x, x => x * x)
    checkFunction(x => math.log(math.abs(x)), x => x.abs.log)
    checkFunction(x => math.log(math.abs(math.log(math.abs(x)))), x => x.abs.log.abs.log)
    checkFunction(x => math.abs(x), x => x.abs)
    checkFunction(x => x, x => x)
  }

  def checkFunction(f: Double => Double, fd2: D2V => D2V): Unit = {
    val values = Array(-2.1345, -1.432, -0.1, 0.315, 1.618)
    for (x <- values) {
      val ff = fd2(D2V(x, 1))
      println()
      println(x + "\t" + ff)
      println(f(x) + "\t" + deriv(f, x) + "\t" + d2(f, x))
      assert(f(x) == ff.v)
      assert(math.abs(deriv(f, x) - ff.d) < eps)
      assert(math.abs(d2(f, x) - ff.d2) < 100 * eps2)
    }
  }

}
