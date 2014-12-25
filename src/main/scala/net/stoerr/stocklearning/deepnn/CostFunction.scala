package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DValue

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 24.12.2014
 */
trait CostFunction {

  val inputSize: Int
  val extraInputSize: Int

  def apply(extraInputs: Array[Double])(inputs: Array[Double]): (Double, Array[Double])

}

abstract class DValueCostFunction(val vars: Array[String], override val extraInputSize: Int) extends CostFunction {

  override val inputSize: Int = vars.size

  def dvfunc(extraInputs: Array[Double])(inputs: Array[Double]): DValue

  override def apply(extraInputs: Array[Double])(inputs: Array[Double]): (Double, Array[Double]) = {
    val res = dvfunc(extraInputs)(inputs)
    (res.value, vars.map(res.deriv(_)))
  }

}

/** Cost function for standard learning to approximate some expected outputs */
class ApproximateCostFunction(override val inputSize: Int) extends DValueCostFunction(
  (0 until inputSize).map("v" + _).toArray, inputSize
) {

  override def dvfunc(extraInputs: Array[Double])(inputs: Array[Double]): DValue = {
    (inputs, vars, extraInputs).zipped.map((i, v, ei) => DValue(i, v) - DValue(ei)).map(x => x * x).reduce(_ + _)
  }

}
