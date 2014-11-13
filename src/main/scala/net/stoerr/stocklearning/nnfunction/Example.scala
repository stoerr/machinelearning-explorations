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

  import scala.language.postfixOps

  private val varnames = (0 until inputs.length) map ("v" + _) toArray

  override def gain(outputValues: Array[Double]): Double = func(outputValues.map(DValue(_))).value

  override def gainWithGradient(outputValues: Array[Double]): (Double, Array[Double]) = {
    val args = (outputValues, varnames).zipped.map(DValue(_, _))
    val fval = func(args)
    (fval.value, varnames.map(fval.deriv(_)))
  }


}
