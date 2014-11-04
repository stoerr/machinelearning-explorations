package net.stoerr.stocklearning

import StockQuoteRepository._

object OptionStrategyExample {

  lazy val options = Array(daxCall5000, daxPut5000, daxCall11000, daxPut11000)
  val onames = Array("c5", "p5", "c11", "p11")
}

/**
 * Example to train a neural network to make gains by bying / selling stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 30.10.2014
 * @param offset start index in maps -> <= -1
 */
class OptionStrategyExample(length: Int, offset: Int) {

  import OptionStrategyExample._

  // filled from dax etc.; the length is taken for the neural networks input size.
  val inputs: Array[Double] = ((offset - length) until (offset)).map(StockQuoteRepository.dax).toArray;

  // current prices
  val p: Array[DValue] = options.map(_(offset)).map(DValue(_))

  // tomorrows prices, for evaluation
  val pp: Array[DValue] = options.map(_(offset + 1)).map(DValue(_))

  /** n'_i = (1+o_i)/(p_i sum( (1+o_i) )) , evaluation ln(sum(n'_i p'_i)) */
  def evaluateAndLearn(network: BackpropagatedNeuralNetwork): Double = {
    network.calculate(inputs)
    val o = network.lastLayer.zip(onames).map {
      case (v, n) => DValue(v.output, n)
    }
    // n'_i = (1+o_i)/(p_i sum( (1+o_i) ))
    val sum1poi = o.map(_ + ONE).reduce(_ + _)
    val np = (o, p).zipped map ((oi, pi) => (oi + ONE) / (pi * sum1poi))
    // evaluation ln(sum(n'_i p'_i))
    val valuation : DValue = (np, pp).zipped.map(_ * _).reduce(_ + _).log
    network.lastLayer.zip(onames).map {
      case (v, n) => v.adapt(eps * valuation.deriv(n))
    }
    valuation.value
  }

}
