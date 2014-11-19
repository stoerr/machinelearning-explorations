package net.stoerr.stocklearning.directnn

import net.stoerr.stocklearning.common.{StockQuoteRepository, DValue}
import StockQuoteRepository.StockData
import net.stoerr.stocklearning.java.BackpropagatedNeuralNetwork
import net.stoerr.stocklearning.common.DValue
import net.stoerr.stocklearning.common.DValue.ONE

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
