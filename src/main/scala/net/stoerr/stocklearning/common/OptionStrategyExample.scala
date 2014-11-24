package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DValue._
import net.stoerr.stocklearning.common.StockQuoteRepository.StockData
import net.stoerr.stocklearning.nnfunction.Example

import scala.util.Random

/**
 * Example to train a neural network to make gains by bying / selling stock options.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 30.10.2014
 * @param offset start index in maps -> <= -1
 */
class OptionStrategyExample(length: Int, offset: Int) extends Example {

  private def extractOfStock(stock: StockData): Array[Double] =
    ((offset - length) until offset).map(stock).toArray

  // current prices
  val p: Array[DValue] = StockQuoteRepository.options.map(_(offset)).map(DValue(_))

  // tomorrows prices, for evaluation
  val pp: Array[DValue] = StockQuoteRepository.options.map(_(offset + 1)).map(DValue(_))

  // filled from dax etc.; the length is taken for the neural networks input size.
  val inputs: Array[Double] = extractOfStock(StockQuoteRepository.dax) ++
    StockQuoteRepository.options.map(extractOfStock).reduce(_ ++ _) ++ Array(offset.asInstanceOf[Double])
  // val inputs: Array[Double] = p.map(_.value) ++ pp.map(_.value)

  def theoreticalMaximumGain: Double = {
    val res: Array[Double] = (pp, p).zipped map ((x: DValue, y: DValue) => (x / y).log.value)
    res.reduce((x, y) => math.max(x, y))
  }

  override def gain(outputValues: Array[Double]): Double = dvalueGain(outputValues).value

  override def gainWithGradient(outputValues: Array[Double]): (Double, Array[Double]) = {
    val dvalue = dvalueGain(outputValues)
    (dvalue.value, StockQuoteRepository.onames.map(dvalue.deriv(_)))
  }

  /** n'_i = (1+o_i)/(p_i sum( (1+o_i) )) , evaluation ln(sum(n'_i p'_i)) */
  private def dvalueGain(nnOutputs: Array[Double]): DValue = {
    val o: Array[DValue] = (nnOutputs, StockQuoteRepository.onames).zipped.map(DValue(_, _))
    // n'_i = (1+o_i)/(p_i sum( (1+o_i) ))
    val sum1poi = o.map(_ + ONE).reduce(_ + _)
    val np = (o, p).zipped map ((oi, pi) => (oi + ONE) / (pi * sum1poi))
    // evaluation ln(sum(n'_i p'_i))
    (np, pp).zipped.map(_ * _).reduce(_ + _).log * DValue(-1)
  }

}

trait OptionStrategyExampleSet {

  val historyLength = 50
  val intermediateLayerSize = 100
  val maxRange = StockQuoteRepository.maxIndex - 1
  val minRange = StockQuoteRepository.minIndex + historyLength + 10
  val controlQuotaPercent = 10

  val rangeSplit: Int = (maxRange - minRange) * controlQuotaPercent / 100 + minRange
  val modelExample = new OptionStrategyExample(historyLength, StockQuoteRepository.maxIndex)

  val learnExamples = Random.shuffle(minRange until rangeSplit map (new OptionStrategyExample(historyLength, _)))
  // val learnExamples = Array(modelExample)
  val evalExamples = rangeSplit until maxRange map (new OptionStrategyExample(historyLength, _))

}
