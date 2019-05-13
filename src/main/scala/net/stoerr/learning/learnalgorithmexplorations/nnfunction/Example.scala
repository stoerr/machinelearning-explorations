package net.stoerr.learning.learnalgorithmexplorations.nnfunction

import Example.ValueWithGradient
import net.stoerr.learning.learnalgorithmexplorations.common.DValue

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

  override def toString() = getClass.getSimpleName + inputs.toList
}

class ExampleWithDValueFunction(val inputs: Array[Double], func: Array[DValue] => DValue) extends Example {

  override def gain(outputValues: Array[Double]): Double =
    DValue.asDoubleFunction(func)(outputValues)

  override def gainWithGradient(outputValues: Array[Double]): (Double, Array[Double]) =
    DValue.asDoubleFunctionWithGradient(func)(outputValues)

}

class ExampleForStinoNN(inputs: Array[Double], val expectedOutputs: Array[Double])
  extends ExampleWithDValueFunction(inputs, outputs =>
    (outputs, expectedOutputs.map(DValue(_))).zipped.map(_ - _).map(x => x * x).reduce(_ + _)) {
  override def toString() = "StinoExample{" + inputs.toList + " => " + expectedOutputs.toList + "}"
}
