package net.stoerr

import net.stoerr.stocklearning.BackpropagatedNeuralNetwork

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 24.10.2014
 */
object TryBackpropNetwork extends App {

  val network : BackpropagatedNeuralNetwork = new BackpropagatedNeuralNetwork(1, 1, 1)

  println(network)

}
