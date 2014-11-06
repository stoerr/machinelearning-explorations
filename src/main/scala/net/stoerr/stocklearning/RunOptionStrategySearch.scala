package net.stoerr.stocklearning

import scala.util.Random

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch extends App {

  val eps = 1
  val historyLength = 14
  val intermediateLayerSize = 10
  val maxRange = StockQuoteRepository.maxIndex
  val minRange = StockQuoteRepository.minIndex + historyLength
  val controlQuotaPercent = 10

  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange
  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)
  val nn = new BackpropagatedNeuralNetwork(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val learnExamples = Random.shuffle(minRange until rangeSplit map (new OptionStrategyExample(historyLength, _)))
  val evalExamples = rangeSplit until maxRange map (new OptionStrategyExample(historyLength, _))

  val timer = new Timer("learn")
  for (round <- 0 until 100) {
    val learnStats = new Statistics("learn" + round)
    for (example <- learnExamples) {
      learnStats += example.evaluateAndLearn(nn, eps)
    }
    println(learnStats)
    val evalStats = new Statistics("eval" + round)
    for (example <- evalExamples) {
      evalStats += example.evaluateAndLearn(nn, eps)
    }
    println(evalStats)
  }
  println(timer)

}
