package net.stoerr.learning.learnalgorithmexplorations.common

import Math._

import org.scalatest.funsuite.AnyFunSuite

class TestNumericalMinimumFinder extends AnyFunSuite {

  test("secondordermin") {
    def f(x: Double) = cosh(x - PI)

    val (x, y) = NumericalMinimumFinder.secondorderMinimumSearch(f, 10)
    println((x, y))
    assert(abs(x - PI) < 1e-6)
  }

  test("derivatives") {
    def f(x: Double) = 1 + x + x * x

    def taylerextrapolate(x0: Double, y0: Double, yd: Double, ydd: Double, x: Double): Double
    = y0 + (x - x0) * yd + (x - x0) * (x - x0) * ydd / 2

    {
      val (d1, d2) = NumericalMinimumFinder.derivatives(1, f(1), 0, f(0), 2, f(2))
      assert(abs(d1 - 3) < 1e-6)
      assert(abs(d2 - 2) < 1e-6)
      assert(abs(taylerextrapolate(1, f(1), d1, d2, 4) - f(4)) < 1e-6)
    }
    {
      val (d1, d2) = NumericalMinimumFinder.derivatives(1, f(1), 7, f(7), -4, f(-4))
      assert(abs(d1 - 3) < 1e-6)
      assert(abs(d2 - 2) < 1e-6)
      assert(abs(taylerextrapolate(1, f(1), d1, d2, 5) - f(5)) < 1e-6)
    }
  }

}
