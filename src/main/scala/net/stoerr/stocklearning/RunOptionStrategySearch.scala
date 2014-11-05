package net.stoerr.stocklearning

import scala.collection.immutable.TreeMap

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch {

  def main(args: Array[String]) {
    val ex = new OptionStrategyExample(5, -1)
    val nn = new BackpropagatedNeuralNetwork(ex.inputs.length, 1, ex.p.length)
    for (i <- 0 until 100) {
      println("Gewinn: " + ex.evaluateAndLearn(nn, 1))
      println("Outputs: " + nn.lastLayer.toList.map(_.lastOutput))
      println()
    }
  }

}
