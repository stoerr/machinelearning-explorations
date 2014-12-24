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

  /** R**sizeWeights x R**sizeInputs -> R**sizeOutputs */
  def f(inputs: WrappedArray[Double])(weights: WrappedArray[Double]): WrappedArray[Double] = fg(inputs)(weights)._1

  case class GradInfo(inputGradient: WrappedArray[Double], weightGradient: WrappedArray[Double])

  def fg(inputs: WrappedArray[Double])(weights: WrappedArray[Double]): (WrappedArray[Double], WrappedArray[Double] => GradInfo)

  def |(o: DeepNN): DeepNN = DeepNN.join(this, o)

  /** R**sizeInputs x R**sizeWeights -> (R, R**sizeWeights) */
  def fgrad(inputs: WrappedArray[Double], weights: WrappedArray[Double]): (WrappedArray[Double], WrappedArray[Double]) = {
    assert(1 == sizeOutputs)
    val (y, g) = fg(inputs)(weights)
    (y, g(Array(1.0)).weightGradient)
  }

}

object DeepNN {

  def join(m: DeepNN, n: DeepNN): DeepNN = new DeepNN {
    assert(m.sizeOutputs == n.sizeInputs)

    override val sizeInputs: Int = this.sizeInputs
    override val sizeWeights: Int = m.sizeWeights + n.sizeWeights
    override val sizeOutputs: Int = n.sizeOutputs

    override def fg(inputs: WrappedArray[Double])(weights: WrappedArray[Double]): (WrappedArray[Double], WrappedArray[Double] => GradInfo) = {
      assert(m.sizeInputs == inputs.size)
      assert(sizeWeights == weights.size)
      val (my, mg) = m.fg(inputs)(weights.slice(m.sizeWeights, sizeWeights))
      val (ny, ng) = n.fg(my)(weights.slice(0, m.sizeWeights))
      (ny, outputGradient => {
        val nginfo = ng(outputGradient)
        val mginfo = mg(nginfo.inputGradient)
        GradInfo(inputGradient = mginfo.inputGradient, weightGradient = mginfo.weightGradient ++ nginfo.weightGradient)
      })
    }
  }

}
