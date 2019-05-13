package net.stoerr.learning.gpunn.calculationcompiler

import CalculationTerm._
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.01.2015
 */
class TestCalculationTemplate extends FunSuite {

  test("build template") {
    implicit val tmpl = new CalculationStore
    val inputs = (0 until 4) map (_ => tmpl.newVariable())
    val weights = (0 until 4) map (_ => tmpl.newVariable())
    val intermediate = tmpl.newVariable()
    val output = tmpl.newVariable()
    tmpl ++= (inputs, weights).zipped map (WeightedSumItem(_, _, intermediate))
    tmpl += UnaryFunctionItem("tanh", math.tanh, intermediate, output)
    val t = output * output
    println(t)
    println(tmpl)
    val compiler = tmpl.executionPlan()
    println(compiler)
  }

}
