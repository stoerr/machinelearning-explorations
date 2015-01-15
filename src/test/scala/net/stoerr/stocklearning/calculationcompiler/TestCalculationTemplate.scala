package net.stoerr.stocklearning.calculationcompiler

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.01.2015
 */
class TestCalculationTemplate extends FunSuite {

  test("build template") {
    implicit val tmpl = new CalculationTemplate
    val inputs = (0 until 4) map (_ => tmpl.newInput())
    val weights = (0 until 4) map (_ => tmpl.newInput())
    val intermediate = tmpl.newVariable()
    val output = tmpl.newOutput()
    tmpl ++= (inputs, weights).zipped map (WeightedSum(_, _, intermediate))
    tmpl += Tanh(intermediate, output)
    // val t = output * output
    // println(t)
    println(tmpl)
    val compiler = tmpl.compile()
    println(compiler)
  }

}
