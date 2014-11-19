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

//learn99 = -1.8505474551130363E-4 +- 0.02225999050624452 [ -0.042320499107943846 , 0.07995768208399752 ] : 80
//learnMaxGain99 = 0.0381451897920574 +- 0.03680484491295854 [ -0.0034259645233814416 , 0.20139480845541347 ] : 80
//eval99 = -7.788309363381099E-4 +- 0.01512705717845322 [ -0.0726366217384336 , 0.05612766295308818 ] : 726
//evalMaxGain99 = 0.03236973181538048 +- 0.03116166615866778 [ -0.014939534275945552 , 0.24659521234925932 ] : 726
//learning: 18.778002s
