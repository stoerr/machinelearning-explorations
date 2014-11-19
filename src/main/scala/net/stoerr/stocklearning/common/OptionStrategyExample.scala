package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.StockQuoteRepository.StockData

import scala.util.Random

/**
 * Example to train a neural network to make gains by bying / selling stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 30.10.2014
 * @param offset start index in maps -> <= -1
 */
class OptionStrategyExample(length: Int, offset: Int) {

  def extractOfStock(stock: StockData): Array[Double] =
    ((offset - length) until offset).map(stock).toArray

  // current prices
  val p: Array[DValue] = StockQuoteRepository.options.map(_(offset)).map(DValue(_))

  // tomorrows prices, for evaluation
  val pp: Array[DValue] = StockQuoteRepository.options.map(_(offset + 1)).map(DValue(_))

  // filled from dax etc.; the length is taken for the neural networks input size.
  val inputs: Array[Double] = extractOfStock(StockQuoteRepository.dax) ++
    StockQuoteRepository.options.map(extractOfStock).reduce(_ ++ _) ++ Array(offset.asInstanceOf[Double])
  // val inputs: Array[Double] = p.map(_.value) ++ pp.map(_.value)

  def theoreticalMaximumGain = {
    val res: Array[Double] = (pp, p).zipped map ((x: DValue, y: DValue) => (x / y).log.value)
    res.reduce((x, y) => math.max(x, y))
  }

}

trait OptionStrategyExampleSet {

  val eps = 1
  val historyLength = 30
  val intermediateLayerSize = 100
  val maxRange = StockQuoteRepository.maxIndex - 1
  val minRange = StockQuoteRepository.minIndex + historyLength + 10
  val controlQuotaPercent = 10

  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange
  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)

  val learnExamples = Random.shuffle(minRange until rangeSplit map (new OptionStrategyExample(historyLength, _)))
  // val learnExamples = Array(modelExample)
  val evalExamples = rangeSplit until maxRange map (new OptionStrategyExample(historyLength, _))

}
