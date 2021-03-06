package net.stoerr.learning.learnalgorithmexplorations.nnfunction

import net.stoerr.learning.learnalgorithmexplorations.common.{OptionStrategyExampleSet, RProp, Statistics}
import net.stoerr.learning.learnalgorithmexplorations.common.Timer._
import net.stoerr.learning.learnalgorithmexplorations.common._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 19.11.2014
 */
object RunOptionStrategySearchWithGradientDescent extends App with OptionStrategyExampleSet {

  val nn = new NNasFunction(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val f = nn.joinedWeightFunction(learnExamples)
  val fgrad = nn.joinedWeightFunctionWithGradient(learnExamples)

  val learnMaxgain = new Statistics("learnMaxGain") ++= learnExamples.map(_.theoreticalMaximumGain)
  val evalMaxgain = new Statistics("evalMaxGain") ++= evalExamples.map(_.theoreticalMaximumGain)

  timing("learning") {
    val weights = (0 until nn.sizeWeights).map(_ => 2 * (math.random - 0.5)).toArray

    val (nweights, lastgain, lastchange) = new RProp(f, fgrad, 200, weights).descent()

    val learnStats = nn.statistics("learn", nweights, learnExamples) * -1
    println(learnStats)
    println(learnMaxgain)
    println((lastgain, lastchange))

    val evalStats = nn.statistics("eval", nweights, evalExamples) * -1
    println(evalStats)
    println(evalMaxgain)

    println(nn.outputWeightsStatistics(weights))
    println(nn.outputWeightsStatistics(nweights))
  }

}
