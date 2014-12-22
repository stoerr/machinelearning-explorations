package net.stoerr.stocklearning.deepnn

import scala.collection.mutable.WrappedArray

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 22.12.2014
 */
trait DeepNN {

  val sizeInputs: Int
  val sizeWeights: Int
  val sizeOutputs: Int

  /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs */
  def f(inputs: WrappedArray[Double], weights: WrappedArray[Double]): WrappedArray[Double]

  /** R**sizeInputs x R**sizeWeights -> (R**sizeOutputs, R**sizeWeights) */
  def fgrad(inputs: WrappedArray[Double], weights: WrappedArray[Double]): (WrappedArray[Double], WrappedArray[Double])

  def |(o: DeepNN): DeepNN = DeepNN.join(this, o)

}

object DeepNN {

  def join(m: DeepNN, n: DeepNN): DeepNN = new DeepNN {
    assert(m.sizeOutputs == n.sizeInputs)

    override val sizeInputs: Int = this.sizeInputs

    /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs */
    override def f(inputs: WrappedArray[Double], weights: WrappedArray[Double]): WrappedArray[Double] = {
      assert(m.sizeInputs == inputs.size)
      assert(sizeWeights == weights.size)
      n.f(m.f(inputs, weights.slice(0, m.sizeWeights)), weights.slice(m.sizeWeights, sizeWeights))
    }

    /** R**sizeInputs x R**sizeWeights -> (R**sizeOutputs, R**sizeWeights) */
    override def fgrad(inputs: WrappedArray[Double], weights: WrappedArray[Double]): (WrappedArray[Double], WrappedArray[Double]) = ???

    override val sizeWeights: Int = m.sizeWeights + n.sizeWeights
    override val sizeOutputs: Int = n.sizeOutputs
  }

}
