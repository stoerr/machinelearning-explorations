package net.stoerr.stocklearning.directnn

import net.stoerr.stocklearning.common.DValue._
import net.stoerr.stocklearning.common.Timer._
import net.stoerr.stocklearning.common.{DValue, Statistics, StockQuoteRepository}
import net.stoerr.stocklearning.java.BackpropagatedNeuralNetwork

import scala.util.Random

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch extends App {

  val eps = 1
  val historyLength = 30
  val intermediateLayerSize = 100
  val maxRange = StockQuoteRepository.maxIndex - 1
  val minRange = StockQuoteRepository.minIndex + historyLength + 10
  val controlQuotaPercent = 10

  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange
  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)
  val nn = new BackpropagatedNeuralNetwork(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val learnExamples = Random.shuffle(minRange until rangeSplit map (new OptionStrategyExample(historyLength, _)))
  // val learnExamples = Array(modelExample)
  val evalExamples = rangeSplit until maxRange map (new OptionStrategyExample(historyLength, _))

  def evaluate(network: BackpropagatedNeuralNetwork, ex: OptionStrategyExample): Double = evaluateAndLearn(network, ex, 0)

  /** n'_i = (1+o_i)/(p_i sum( (1+o_i) )) , evaluation ln(sum(n'_i p'_i)) */
  def evaluateAndLearn(network: BackpropagatedNeuralNetwork, ex: OptionStrategyExample, eps: Double): Double = {
    network.calculate(ex.inputs)
    val o = network.lastLayer.zip(StockQuoteRepository.onames).map {
      case (v, n) => DValue(v.lastOutput, n)
    }
    // n'_i = (1+o_i)/(p_i sum( (1+o_i) ))
    val sum1poi = o.map(_ + ONE).reduce(_ + _)
    val np = (o, ex.p).zipped map ((oi, pi) => (oi + ONE) / (pi * sum1poi))
    // evaluation ln(sum(n'_i p'_i))
    val valuation: DValue = (np, ex.pp).zipped.map(_ * _).reduce(_ + _).log
    if (0 != eps) network.lastLayer.zip(StockQuoteRepository.onames).map {
      case (v, n) => v.adapt(eps * valuation.deriv(n))
    }
    valuation.value
  }

  timing("learning")(for (round <- 0 until 500) {
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
