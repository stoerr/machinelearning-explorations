package net.stoerr

import net.stoerr.stocklearning.BackpropagatedNeuralNetwork

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 24.10.2014
 */
object TryBackpropNetwork extends App {

  val network: BackpropagatedNeuralNetwork = new BackpropagatedNeuralNetwork(2, 4, 1)

  val examples: List[(Array[Double], Double)] = List(
    Array(0.0, 0.0) -> 0.8,
    Array(0.0, 1.0) -> 0.2,
    Array(1.0, 0.0) -> 0.2,
    Array(1.0, 1.0) -> 0.8
  )

  val eps = 0.2

  for (i <- 0 to 1000) {
    for ((in, out) <- examples) {
      network.calculate(in)
      network.adapt(eps * (out - network.outputs(0)))
    }
  }

  for ((in, out) <- examples) {
    network.calculate(in)
    println( in.toList + " : " + network.outputs(0) + " - " + out )
  }

  println(network)
}
