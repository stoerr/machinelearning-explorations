package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.common.RProp
import org.scalatest.FunSuite

import scala.util.Random

/**
  * We try a NN with many input examples to explore effects of parallelization.
  * Thinkpad GPU 50000 examples of 20 length 20 min,
  * CPU XOR Cached 1000 examples of 20 length 7 min
  */
class TestNNParallelization extends FunSuite {

  import Math.{cos, sin}

  import Random._

  val numExamples = 1000
  val exampleLength = 20

  val xor = List(
    (List(-1.0, -1.0), List(-0.5)),
    (List(-1.0, 1.0), List(0.5)),
    (List(1.0, -1.0), List(0.5)),
    (List(1.0, 1.0), List(-0.5))
  )

  val examples: List[(List[Double], List[Double])] =
    Range(0, numExamples).toList.flatMap { i =>
      xor.map { case (List(x, y), List(z)) =>
        (Range(0, exampleLength).toList.map(j =>
          x * cos(j) + y * sin(j) + nextGaussian()
        ), List(z))
      }
    }

  if (numExamples <= 1000) ignore("Learn XOR Cached") {
    def nn = NNCreator.simpleNetwork(List(exampleLength, 7, 7, 1))
    val wfunc: Array[Double] => (Double, Array[Double]) = NNCachedCalculationStrategy.asDerivedFunction(nn
      .evaluationTerm, examples)
    val startWeights = nn.weights.indices.map(_ => Random.nextGaussian() / 20).toArray
    val rprop = new RProp(wfunc.andThen(_._1), wfunc, 100, startWeights)
    wfunc(startWeights) // warm up
    val begin = System.currentTimeMillis()
    val result = rprop.descent()
    println(s"Time: ${System.currentTimeMillis() - begin} ms")
    println(result)
    val weights = result._1
    println(weights.toList)
    assert(nn.outputCalculations.size == 1)
    /* val nnfunc = NNCachedCalculationStrategy.asResultFunction(nn.outputCalculations.head, weights)
    for (example <- examples) {
      println(example._1 + ":" + nnfunc(example._1.toArray) + " for " + example._2)
    } */
  }

  ignore("Learn XOR Transpiler") {
    def nn = NNCreator.simpleNetwork(List(exampleLength, 7, 7, 1))
    val wfunc: Array[Double] => (Double, Array[Double]) = new NNTranspilerCalculationStrategy(nn
      .usedToplevelNNTerms)
      .asDerivedFunction(nn.evaluationTerm, examples)
    val startWeights = nn.weights.indices.map(_ => Random.nextGaussian() / 20).toArray
    val rprop = new RProp(wfunc.andThen(_._1), wfunc, 100, startWeights)
    wfunc(startWeights) // warm up
    val begin = System.currentTimeMillis()
    val result = rprop.descent()
    println(s"Time: ${System.currentTimeMillis() - begin} ms")
    println(result)
    val weights = result._1
    println(weights.toList)
    assert(nn.outputCalculations.size == 1)
    /* val nnfunc = NNCachedCalculationStrategy.asResultFunction(nn.outputCalculations.head, weights)
    for (example <- examples) {
      println(example._1 + ":" + nnfunc(example._1.toArray) + " for " + example._2)
    } */
  }

}
