package net.stoerr.learning.learnalgorithmexplorations.deepnn2

import net.stoerr.learning.learnalgorithmexplorations.common.{DoubleArrayVector, RProp}
import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector._
import net.stoerr.learning.learnalgorithmexplorations.common.RProp
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.TreeSet
import scala.util.Random

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 10.09.2015
 */
class TestNNCreator extends AnyFunSuite {

  val xorExample: List[(List[Double], List[Double])] =
    Range(0, 2).toList.flatMap(i =>
      List(
        (List(-1.0, -1.0), List(-0.5)),
        (List(-1.0, 1.0), List(0.5)),
        (List(1.0, -1.0), List(0.5)),
        (List(1.0, 1.0), List(-0.5))
      ))

  def nnterms(terms: Seq[NNTermBase]): Array[NNTerm] = terms.filter(_.isInstanceOf[NNTerm]).map(_
    .asInstanceOf[NNTerm]).toArray

  def uniqsize(terms: Seq[NNTerm]): Int = (new TreeSet[NNTerm]() ++ terms).size

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

    val dif = (DoubleArrayVector.gradient(w1.andThen(_._1))(arg) - x1._2).abs
    println(dif)
    assert(dif < eps)
    assert(math.abs(x1._1 - x2._1) < eps)
  }

  test("checkTranspiler") {
    def nn22 = NNCreator.simpleNetwork(List(2, 2, 2, 1))

    println(nn22.evaluationTerm.inputs.toList)
    println(nn22.evaluationTerm.outputs.toList)
    println(nn22.evaluationTerm.weights.toList)
    val w1 = NNCachedCalculationStrategy.asDerivedFunction(nn22.evaluationTerm, xorExample)
    val arg = (1 to nn22.evaluationTerm.weights.length).toArray.map(_ / 50.0)
    val x1 = w1(arg)
    println(x1)
    println(x1._2.toList)

    val w2 = new NNTranspilerCalculationStrategy(nn22.usedToplevelNNTerms).asDerivedFunction(nn22.evaluationTerm,
      xorExample)
    val x2 = w2(arg)
    println(x2)
    println(x2._2.toList)

    val dif = (DoubleArrayVector.gradient(w1.andThen(_._1))(arg) - x1._2).abs
    println(dif)
    assert(dif < eps)
    assert(math.abs(x1._1 - x2._1) < eps)
  }

  test("Learn XOR Cached") {
    def nn2331 = NNCreator.simpleNetwork(List(2, 9, 9, 1))
    val wfunc: Array[Double] => (Double, Array[Double]) = NNCachedCalculationStrategy.asDerivedFunction(nn2331
      .evaluationTerm, xorExample)
    val startWeights = nn2331.weights.indices.map(_ => Random.nextGaussian() / 20).toArray
    val rprop = new RProp(wfunc.andThen(_._1), wfunc, 100, startWeights)
    wfunc(startWeights) // warm up
    val begin = System.currentTimeMillis()
    val result = rprop.descent()
    println(s"Time: ${System.currentTimeMillis() - begin} ms")
    println(result)
    val weights = result._1
    println(weights.toList)
    assert(nn2331.outputCalculations.size == 1)
    val nnfunc = NNCachedCalculationStrategy.asResultFunction(nn2331.outputCalculations.head, weights)
    for (example <- xorExample) {
      println(example._1 + ":" + nnfunc(example._1.toArray) + " for " + example._2)
    }
  }

  test("Learn XOR Transpiler") {
    def nn2331 = NNCreator.simpleNetwork(List(2, 9, 9, 1))
    val wfunc: Array[Double] => (Double, Array[Double]) = new NNTranspilerCalculationStrategy(nn2331
      .usedToplevelNNTerms)
      .asDerivedFunction(nn2331.evaluationTerm, xorExample)
    val startWeights = nn2331.weights.indices.map(_ => Random.nextGaussian() / 20).toArray
    val rprop = new RProp(wfunc.andThen(_._1), wfunc, 100, startWeights)
    wfunc(startWeights) // warm up
    val begin = System.currentTimeMillis()
    val result = rprop.descent()
    println(s"Time: ${System.currentTimeMillis() - begin} ms")
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
