package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.nnfunction.Example.ValueWithGradient

object Example {
  type ValueWithGradient = (Double, Array[Double])
}

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 13.11.2014
 */
trait Example {

  val inputs: Array[Double]

  def gain(outputValues: Array[Double]): Double

  def gainWithGradient(outputValues: Array[Double]): ValueWithGradient
}
