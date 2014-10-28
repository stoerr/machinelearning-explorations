package net.stoerr.stocklearning

/**
 * Adapter to use a neural network as a strategy for buying stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.10.2014
 */
case class NNStockAdapter(network: BackpropagatedNeuralNetwork) {

  val numOpt = network.lastLayer.size

  def optionVolumes() = {

  }

}
