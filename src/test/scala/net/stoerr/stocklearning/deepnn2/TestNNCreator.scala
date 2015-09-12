package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.common.DoubleArrayVector
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 10.09.2015
 */
class TestNNCreator extends FunSuite {

  test("learn xor") {
    def nn22 = NNCreator.simpleNetwork(List(2, 2, 1))
    val inout = List(
      (List(0.0, 0.0), List(-0.5)),
      (List(0.0, 1.0), List(0.5)),
      (List(1.0, 0.0), List(0.5)),
      (List(1.0, 1.0), List(-0.5))
    )
    println(nn22.evaluationTerm.inputs.toList)
    println(nn22.evaluationTerm.outputs.toList)
    println(nn22.evaluationTerm.weights.toList)
    val w1 = NNCachedCalculationStrategy.asDerivedFunction(nn22.evaluationTerm, inout)
    val x1 = w1(Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    println(x1)
    println(x1._2.toList)
    val w2 = NNSimpleCalculationStrategy.asDerivedFunction(nn22.evaluationTerm, inout)
    val x2 = w2(Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    println(x2)
    println(x2._2.toList)
    val arg3 = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
    val x3 = w2(arg3)
    println(x3)
    println(x3._2.toList)
    println(DoubleArrayVector.gradient(w2.andThen(_._1), arg3).toList)
  }

}
