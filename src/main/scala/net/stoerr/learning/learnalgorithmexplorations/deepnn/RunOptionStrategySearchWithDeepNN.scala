package net.stoerr.learning.learnalgorithmexplorations.deepnn

import net.stoerr.learning.learnalgorithmexplorations.common.{OptionStrategyExampleSet, RProp, Statistics}
import net.stoerr.learning.learnalgorithmexplorations.common.Timer._
import net.stoerr.learning.learnalgorithmexplorations.common._

import scala.util.Random


/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearchWithDeepNN extends App with OptionStrategyExampleSet {

  val eps = -1
  val nn = DeepNNLayers.basicNN(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  timing("learning")(for (round <- 0 until 10) {
    var weights = Array.fill(nn.sizeWeights)(Random.nextDouble() * 0.001 - 0.0005)

    val f = nn.fCombined(learnExamples) _
    val fgrad = nn.fgradCombined(learnExamples) _
    val (wbest, ybest, lasteps) = new RProp(f, fgrad, 100, weights).descent()
    println((wbest, ybest, lasteps))
    weights = wbest

    val learnStats = new Statistics("learn" + round)
    val learnMaxgain = new Statistics("learnMaxGain" + round)
    for (example <- learnExamples) {
      learnStats += nn.f(example)(weights)
      learnMaxgain += example.theoreticalMaximumGain
    }
    println(learnStats)
    println(learnMaxgain)

    val evalMaxgain = new Statistics("evalMaxGain" + round)
    val evalStats = new Statistics("eval" + round)
    for (example <- evalExamples) {
      evalStats += nn.f(example)(weights)
      evalMaxgain += example.theoreticalMaximumGain
    }
    println(evalStats)
    println(evalMaxgain)
  })

  // println(nn)

}
