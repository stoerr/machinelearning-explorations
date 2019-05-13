package net.stoerr.learning.learnalgorithmexplorations.nnfunction

import net.stoerr.learning.learnalgorithmexplorations.common.{Competition, OptionStrategyExampleSet, RProp, Statistics}
import net.stoerr.learning.learnalgorithmexplorations.common.Timer._
import net.stoerr.learning.learnalgorithmexplorations.common._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 19.11.2014
 */
object RunCompetitiveOptionStrategySearchWithGradientDescent extends App
with OptionStrategyExampleSet with Competition[Array[Double]] {

  val nn = new NNasFunction(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val f = nn.joinedWeightFunction(learnExamples)
  val fgrad = nn.joinedWeightFunctionWithGradient(learnExamples)

  val learnMaxgain = new Statistics("learnMaxGain") ++= learnExamples.map(_.theoreticalMaximumGain)
  val evalMaxgain = new Statistics("evalMaxGain") ++= evalExamples.map(_.theoreticalMaximumGain)

  override def makeCompetitor(): Array[Double] = (0 until nn.sizeWeights).map(_ => 2 * (math.random - 0.5)).toArray

  override def eval(weights: Array[Double]): Double = -f(weights)

  override def train(weights: Array[Double]): Array[Double] =
    new RProp(f, fgrad, 50, weights).descent()._1

  timing("learning") {
    println(learnMaxgain)
    println(evalMaxgain)

    val bestExample = compete(10, 50)

    println("=================================")
    val learnStats = nn.statistics("learn", bestExample, learnExamples) * -1
    println(learnStats)
    println(learnMaxgain)

    val evalStats = nn.statistics("eval", bestExample, evalExamples) * -1
    println(evalStats)
    println(evalMaxgain)

    println(nn.outputWeightsStatistics(bestExample))
    println(nn.outputWeightsStatistics(bestExample))
  }

}
