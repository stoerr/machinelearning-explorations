package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DoubleArrayVector._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.12.2014
 */
abstract class SummingLayer(override val sizeInputs: Int, override val sizeOutputs: Int) extends DeepNN with ActivationFunction {
  override val sizeWeights: Int = sizeInputs * sizeOutputs

  /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs , R**sizeOutputs => GradInfo */
  override def fg(inputs: Array[Double])(weights: Array[Double]): (Array[Double], (Array[Double]) => GradInfo) = {
    assert(inputs.size == sizeInputs)
    assert(weights.size == sizeWeights)
    val outputs = (for (o <- 0 until sizeOutputs) yield
      activation((0 until sizeInputs).map(i => inputs(i) * weights(o * sizeInputs + i)).reduce(_ + _))
      ).toArray
    return (outputs, { outGrad: Array[Double] =>
      assert(outGrad.size == sizeOutputs)
      val outbase = outputs.map(derivActivation) elem_* outGrad
      GradInfo(
        inputGradient = (0 until sizeInputs).map { i =>
          (0 until sizeOutputs).map(o => outbase(o) * weights(o * sizeInputs + i)).reduce(_ + _)
        }.toArray,
        weightGradient = (for (o <- 0 until sizeOutputs; i <- 0 until sizeInputs) yield outbase(o) * inputs(i)).toArray
      )
    }
      )
  }

}

trait ActivationFunction {
  def activation(sum: Double): Double

  /** derivation of the activation function after its argument, calculated as function of its result */
  def derivActivation(output: Double): Double
}

trait TanhActivation extends ActivationFunction {
  override def activation(sum: Double): Double = math.tanh(sum)

  override def derivActivation(output: Double): Double = 1 - output * output
}

trait LinearActivation extends ActivationFunction {
  override def activation(sum: Double): Double = sum

  override def derivActivation(output: Double): Double = 1
}

object DeepNNLayers {
  def twoLayerNN(sizeInput: Int, sizeHidden: Int, sizeOutput: Int) =
    new SummingLayer(sizeInput, sizeHidden) with TanhActivation |
      new SummingLayer(sizeHidden, sizeOutput) with TanhActivation
}
