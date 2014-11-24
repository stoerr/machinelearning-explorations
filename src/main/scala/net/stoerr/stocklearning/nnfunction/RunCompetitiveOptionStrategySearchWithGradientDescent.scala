package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.Timer._
import net.stoerr.stocklearning.common._

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

  override def makeCompetitor(): Array[Double] = (0 until nn.dimension).map(_ => 2 * (math.random - 0.5)).toArray

  override def eval(weights: Array[Double]): Double = -f(weights)

  override def train(weights: Array[Double]): Array[Double] =
    new GradientDescentWithWithMinimumApproximation(f, fgrad, 10, weights, -0.01).descent()._1

  timing("learning") {
    println(learnMaxgain)
    println(evalMaxgain)

    val bestExample = compete(100, 100)

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
