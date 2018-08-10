package net.stoerr.stocklearning.deepnn2

import NNTerm._
import SNNTerm._
import net.stoerr.stocklearning.common.DoubleArrayVector.eps
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */
class TestNNTerm extends FunSuite {

  def assertAlmostEqual(x: Double, y: Double): Unit = assert(math.abs(x - y) < eps)

  val inputval1: PartialFunction[NNTerm, Double] = {
    case I("2") => 1.0
    case O("1") => 2.0
  }

  val inputval2: PartialFunction[NNTerm, Double] = {
    case I("2") => 4.0
    case O("1") => 3.0
  }

  val restVal: PartialFunction[NNTerm, Double] = {
    case W("1") => 3.0
  }

  val t1 = W("1") * I("2") + 3.0 - O("1")
  test("NNTerm.eval") {
    assert("(-1.0 * O1 + 3.0 + I2 * W1)" == t1.toString)
    assertAlmostEqual(4, NNSimpleCalculationStrategy.eval(inputval1 orElse restVal)(t1))
    assertAlmostEqual(12, NNSimpleCalculationStrategy.eval(inputval2 orElse restVal)(t1))
    assertAlmostEqual(4, NNCachedCalculationStrategy.eval(inputval1 orElse restVal)(t1))
    assertAlmostEqual(12, NNCachedCalculationStrategy.eval(inputval2 orElse restVal)(t1))
  }

  val t2 = SUMMED(t1) - 2.5
  test("SNNTerm.eval") {
    assert("(-1.0 * 2.5 + SUMMED((-1.0 * O1 + 3.0 + I2 * W1)))" == t2.toString)
    assertAlmostEqual(13.5, NNSimpleCalculationStrategy.eval(List(inputval1, inputval2), restVal)(t2))
    assertAlmostEqual(13.5, NNCachedCalculationStrategy.eval(List(inputval1, inputval2), restVal)(t2))
  }

  test("Derivatives") {
    assert(Map(W("1") -> I("2")) == t1.wDerivative)
    assert(Map(W("1") -> SUMMED(I("2"))) == t2.wDerivative)
  }

  ignore("Network creation") {
    assert("Vector(Tanh((I00 * W03-00-01 + I01 * W03-01-01)), Tanh((I00 * W03-00-02 + I01 * W03-01-02)))" ==
      NNCreator.wireup
        (NNCreator.inputs(2), 2, 3).toString())
    assert("NNRepresentation(Vector(I00, I01),Vector(W01-00-01, W01-01-01, W01-00-02, W01-01-02),Vector(O00, O01)," +
      "Vector(Tanh((I00 * W01-00-01 + I01 * W01-01-01)), Tanh((I00 * W01-00-02 + I01 * W01-01-02))),SUMMED((Sqr((-1.0" +
      " * O00 + Tanh((I00 * W01-00-01 + I01 * W01-01-01)))) + Sqr((-1.0 * O01 + Tanh((I00 * W01-00-02 + I01 * " +
      "W01-01-02)))))))" == NNCreator
      .simpleNetwork(List(2, 2)).toString)
    assert("NNRepresentation(Vector(I00),Vector(W01-00-01, W02-00-01, W03-00-01),Vector(O00),Vector(Tanh((Tanh((Tanh(" +
      "(I00 * W01-00-01)) * W02-00-01)) * W03-00-01))),SUMMED(Sqr((-1.0 * O00 + Tanh((Tanh((Tanh((I00 * W01-00-01)) *" +
      " W02-00-01)) * W03-00-01))))))" == NNCreator
      .simpleNetwork(List(1, 1, 1, 1)).toString)
    val nn = NNCreator.simpleNetwork(List(3, 5, 4))
    assert(3 == nn.inputs.size)
    assert(4 == nn.outputs.size)
    assert(35 == nn.weights.size)
  }

}
