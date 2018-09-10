package net.stoerr.stocklearning.common

import Math._

import org.scalatest.FunSuite

class TestNumericalMinimumFinder extends FunSuite {

  test("secondordermin") {
    def f(x: Double) = cosh(x - PI)

    val (x, y) = NumericalMinimumFinder.secondorderMinimumSearch(f, 5, 10)
    println((x, y))
    assert(abs(x - PI) < 1e-6)
  }

}
