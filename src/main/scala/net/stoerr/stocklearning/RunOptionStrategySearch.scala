package net.stoerr.stocklearning

import net.stoerr.stocklearning.Timer._

import scala.util.Random

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch extends App {

  val eps = 1
  val historyLength = 30
  val intermediateLayerSize = 500
  val maxRange = StockQuoteRepository.maxIndex - 1
  val minRange = StockQuoteRepository.minIndex + historyLength + 10
  val controlQuotaPercent = 10

  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange
  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)
  val nn = new BackpropagatedNeuralNetwork(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val learnExamples = Random.shuffle(minRange until rangeSplit map (new OptionStrategyExample(historyLength, _)))
  // val learnExamples = Array(modelExample)
  val evalExamples = rangeSplit until maxRange map (new OptionStrategyExample(historyLength, _))

  timing("learning")(for (round <- 0 until 10000) {
    val learnStats = new Statistics("learn" + round)
    val learnMaxgain = new Statistics("learnMaxGain" + round)
    for (example <- learnExamples) {
      learnStats += example.evaluateAndLearn(nn, eps)
      learnMaxgain += example.theoreticalMaximumGain
    }
    println(learnStats)
    println(learnMaxgain)

    //    val evalMaxgain = new Statistics("evalMaxGain" + round)
    //    val evalStats = new Statistics("eval" + round)
    //    for (example <- evalExamples) {
    //      evalStats += example.evaluateAndLearn(nn, eps)
    //      evalMaxgain += example.theoreticalMaximumGain
    //    }
    //    println(evalStats)
    //    println(evalMaxgain)
  })

  // println(nn)

}
