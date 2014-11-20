package net.stoerr.stocklearning.directnn

import net.stoerr.stocklearning.common.DValue._
import net.stoerr.stocklearning.common.Timer._
import net.stoerr.stocklearning.common._
import net.stoerr.stocklearning.java.BackpropagatedNeuralNetwork

import scala.util.Random

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearchWithBackpropNN extends App with OptionStrategyExampleSet {

  val eps = -1
  val nn = new BackpropagatedNeuralNetwork(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  def evaluate(network: BackpropagatedNeuralNetwork, ex: OptionStrategyExample): Double = evaluateAndLearn(network, ex, 0)

  def evaluateAndLearn(network: BackpropagatedNeuralNetwork, ex: OptionStrategyExample, eps: Double): Double = {
    network.calculate(ex.inputs)
    val nnOutputs: Array[Double] = network.lastLayer.map(_.lastOutput)
    val (v, grad) = ex.gainWithGradient(nnOutputs)
    (network.lastLayer, grad).zipped.foreach((n, d) => n.adapt(eps*d))
    v
  }

  timing("learning")(for (round <- 0 until 100) {
    val learnStats = new Statistics("learn" + round)
    val learnMaxgain = new Statistics("learnMaxGain" + round)
    for (example <- learnExamples) {
      learnStats += evaluateAndLearn(nn, example, eps)
      learnMaxgain += example.theoreticalMaximumGain
    }
    println(learnStats)
    println(learnMaxgain)

    val evalMaxgain = new Statistics("evalMaxGain" + round)
    val evalStats = new Statistics("eval" + round)
    for (example <- evalExamples) {
      evalStats += evaluateAndLearn(nn, example, eps)
      evalMaxgain += example.theoreticalMaximumGain
    }
    println(evalStats)
    println(evalMaxgain)
  })

  // println(nn)

}
