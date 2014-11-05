package net.stoerr.stocklearning

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch extends App {

  val historyLength = 5
  val intermediateLayerSize = 1
  val maxRange = StockQuoteRepository.maxIndex
  val minRange = StockQuoteRepository.minIndex + historyLength
  val controlQuotaPercent = 10
  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange

  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)
  val nn = new BackpropagatedNeuralNetwork(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)


  for (i <- 0 until 100) {
    println("Gewinn: " + modelExample.evaluateAndLearn(nn, 1))
    println("Outputs: " + nn.lastLayer.toList.map(_.lastOutput))
    println()
  }

}
