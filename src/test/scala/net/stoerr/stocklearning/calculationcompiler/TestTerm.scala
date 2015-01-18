package net.stoerr.stocklearning.calculationcompiler

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.01.2015
 */
class TestTerm extends FunSuite {

  test("just try it") {
    val x = Variable("x")
    val y = Variable("y")
    val f = (x + y) * x
    println(f)
    val deriv = f.totalDerivative
    println(deriv)
  }

}
