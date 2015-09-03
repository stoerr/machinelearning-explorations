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

}
