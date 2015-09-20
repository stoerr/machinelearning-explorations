package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.common.{DoubleArrayVector, RProp}
import DoubleArrayVector._
import org.scalatest.FunSuite

import collection.immutable.TreeSet
import util.Random

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 10.09.2015
 */
class TestNNCreator extends FunSuite {

  val xorExample = List(
    (List(0.0, 0.0), List(-0.5)),
    (List(0.0, 1.0), List(0.5)),
    (List(1.0, 0.0), List(0.5)),
    (List(1.0, 1.0), List(-0.5))
  )

  def nnterms(terms: Seq[NNTermBase]): Array[NNTerm] = terms.filter(_.isInstanceOf[NNTerm]).map(_
    .asInstanceOf[NNTerm]).toArray

  def uniqsize(terms: Seq[NNTerm]) = (new TreeSet[NNTerm]() ++ terms).size

  test("complexities 1") {
    val nn = NNCreator.simpleNetwork(List(2, 5, 1))
    println("2,5,1: " + nnterms(List(nn.evaluationTerm)).length + " / " + uniqsize(nnterms(List(nn.evaluationTerm))))
    val allcomponents = nnterms(nn.evaluationTerm.componentStream ++ nn.evaluationTerm.wDerivative.values.flatMap(_
      .componentStream))
    println("2,5,1 deriv: " + allcomponents.length + " / " + uniqsize(allcomponents))
  }

  test("complexities 2") {
    val nn = NNCreator.simpleNetwork(List(2, 5, 5, 1))
    println("2,5,5,1: " + nnterms(List(nn.evaluationTerm)).length + " / " + uniqsize(nnterms(List(nn.evaluationTerm))))
    val allcomponents = nnterms(nn.evaluationTerm.componentStream ++ nn.evaluationTerm.wDerivative.values.flatMap(_
      .componentStream))
    println("2,5,5,1 deriv: " + allcomponents.length + " / " + uniqsize(allcomponents))
  }

  test("roughCheckSimpleNetwork") {
    def nn22 = NNCreator.simpleNetwork(List(2, 2, 2, 1))

    println(nn22.evaluationTerm.inputs.toList)
    println(nn22.evaluationTerm.outputs.toList)
    println(nn22.evaluationTerm.weights.toList)
    val w1 = NNCachedCalculationStrategy.asDerivedFunction(nn22.evaluationTerm, xorExample)
    val arg = (1 to nn22.evaluationTerm.weights.length).toArray.map(_ / 50.0)
    val x1 = w1(arg)
    println(x1)
    println(x1._2.toList)
    val w2 = NNSimpleCalculationStrategy.asDerivedFunction(nn22.evaluationTerm, xorExample)
    val x2 = w2(arg)
    println(x2)
    println(x2._2.toList)

    val dif = (DoubleArrayVector.gradient(w1.andThen(_._1), arg) - x1._2).abs
    println(dif)
    assert(dif < eps)
    assert(math.abs(x1._1 - x2._1) < eps)
  }

  test("Learn XOR") {
    def nn2331 = NNCreator.simpleNetwork(List(2, 5, 5, 1))
    val wfunc: Array[Double] => (Double, Array[Double]) = NNCachedCalculationStrategy.asDerivedFunction(nn2331
      .evaluationTerm, xorExample)
    val startWeights = nn2331.weights.indices.map(_ => Random.nextGaussian() / 20).toArray
    val rprop = new RProp(wfunc.andThen(_._1), wfunc, 100, startWeights)
    val result = rprop.descent()
    println(result)
    val weights = result._1
    println(weights.toList)
    assert(nn2331.outputCalculations.size == 1)
    val nnfunc = NNCachedCalculationStrategy.asResultFunction(nn2331.outputCalculations.head, weights)
    for (example <- xorExample) {
      println(example._1 + ":" + nnfunc(example._1.toArray) + " for " + example._2)
    }
  }

}
