package net.stoerr.stocklearning

/**
 * Example to train a neural network to make gains by bying / selling stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 30.10.2014
 * @param offset start index in maps -> <= -1
 */
class OptionStrategyExample(length: Int, offset: Int) {

  // filled from dax etc.; the length is taken for the neural networks input size.
  val inputs: Array[Double] = ((offset - length) until (offset)).map(StockQuoteRepository.dax).toArray;

  // current prices
  val p : Array[DValue] = Array(StockQuoteRepository.daxCall5000(offset), StockQuoteRepository.daxPut5000(offset),
    StockQuoteRepository.daxCall11000(offset), StockQuoteRepository.daxPut11000(offset)).map(DValue(_))

  // tomorrows prices, for evaluation
  val pp : Array[DValue] = Array(StockQuoteRepository.daxCall5000(offset), StockQuoteRepository.daxPut5000(offset),
    StockQuoteRepository.daxCall11000(offset), StockQuoteRepository.daxPut11000(offset)).map(DValue(_))

  val onames = Array("c5", "p5", "c11", "p11")

  def evaluate(network : BackpropagatedNeuralNetwork) = {
    network.calculate(inputs)
    val o = network.lastLayer.zip(onames).map(DValue(_.output, _))

  }

}
