package net.stoerr.stocklearning

import net.stoerr.stocklearning.DValue.ONE
import net.stoerr.stocklearning.StockQuoteRepository.StockData

/**
 * Example to train a neural network to make gains by bying / selling stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 30.10.2014
 * @param offset start index in maps -> <= -1
 */
class OptionStrategyExample(length: Int, offset: Int) {

  // filled from dax etc.; the length is taken for the neural networks input size.
  val inputs: Array[Double] = extractOfStock(StockQuoteRepository.dax) ++
    StockQuoteRepository.options.map(extractOfStock).reduce(_ ++ _) ++ Array(offset.asInstanceOf[Double])

  def extractOfStock(stock: StockData): Array[Double] =
    ((offset - length) until offset).map(stock).toArray

  // current prices
  val p: Array[DValue] = StockQuoteRepository.options.map(_(offset)).map(DValue(_))

  // tomorrows prices, for evaluation
  val pp: Array[DValue] = StockQuoteRepository.options.map(_(offset + 1)).map(DValue(_))

  def evaluate(network: BackpropagatedNeuralNetwork): Double = evaluateAndLearn(network, 0)

  /** n'_i = (1+o_i)/(p_i sum( (1+o_i) )) , evaluation ln(sum(n'_i p'_i)) */
  def evaluateAndLearn(network: BackpropagatedNeuralNetwork, eps: Double): Double = {
    network.calculate(inputs)
    val o = network.lastLayer.zip(StockQuoteRepository.onames).map {
      case (v, n) => DValue(v.lastOutput, n)
    }
    // n'_i = (1+o_i)/(p_i sum( (1+o_i) ))
    val sum1poi = o.map(_ + ONE).reduce(_ + _)
    val np = (o, p).zipped map ((oi, pi) => (oi + ONE) / (pi * sum1poi))
    // evaluation ln(sum(n'_i p'_i))
    val valuation: DValue = (np, pp).zipped.map(_ * _).reduce(_ + _).log
    if (0 != eps) network.lastLayer.zip(StockQuoteRepository.onames).map {
      case (v, n) => v.adapt(eps * valuation.deriv(n))
    }
    valuation.value
  }

  def theoreticalMaximumGain = {
    val res: Array[Double] = (pp, p).zipped map ((x: DValue, y: DValue) => (x / y).log.value)
    res.reduce((x, y) => math.max(x, y))
  }

}
