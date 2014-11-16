package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DValue
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

class ExampleWithDValueFunction(val inputs: Array[Double], func: Array[DValue] => DValue) extends Example {

  override def gain(outputValues: Array[Double]): Double =
    DValue.asDoubleFunction(func)(outputValues)

  override def gainWithGradient(outputValues: Array[Double]): (Double, Array[Double]) =
    DValue.asDoubleFunctionWithGradient(func)(outputValues)

}

class ExampleForStinoNN(inputs: Array[Double], val expectedOutputs: Array[Double])
  extends ExampleWithDValueFunction(inputs, outputs =>
    (outputs, expectedOutputs.map(DValue(_))).zipped.map(_ - _).map(x => x * x).reduce(_ + _))
