package net.stoerr.stocklearning.calculationcompiler

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.01.2015
 */
class TestCalculationTemplate extends FunSuite {

  test("build template") {
    val tmpl = new CalculationTemplate
    val inputs = (0 until 2) map (_ => tmpl.newInput())
    val intermediate = tmpl.newVariable()
    val output = tmpl.newOutput()
    tmpl ++= inputs map (Sum(_, intermediate))
    tmpl += Cosh(intermediate, output)
    println(tmpl)
  }

}
