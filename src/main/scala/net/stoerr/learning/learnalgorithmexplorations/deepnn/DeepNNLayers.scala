package net.stoerr.learning.learnalgorithmexplorations.deepnn

import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector._

/** A single layer of a DeepNN as special case */
abstract class DeepNNLayer extends DeepNN {

  def layers: List[DeepNNLayer] = List(this)

}

/**
 * The usual neuron that sums its inputs times its weights, plus a special weight of constant input.
 * The activation function can be mixed in via ActivationFunction trait.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.12.2014
 */
abstract class SummingLayer(override val sizeInputs: Int, override val sizeOutputs: Int) extends DeepNNLayer with ActivationFunction {
  override val sizeWeights: Int = (sizeInputs + 1) * sizeOutputs

  /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs , R**sizeOutputs => GradInfo */
  override def fg(inputs: Array[Double])(weights: Array[Double]): (Array[Double], (Array[Double]) => GradInfo) = {
    assert(inputs.size == sizeInputs)
    assert(weights.size == sizeWeights)
    val perneuron = sizeInputs + 1
    val outputs = (for (o <- 0 until sizeOutputs) yield
      activation((0 until sizeInputs).map(i => inputs(i) * weights(o * perneuron + i + 1)).reduce(_ + _)
        + weights(o * perneuron))
      ).toArray
    return (outputs, { outGrad: Array[Double] =>
      assert(outGrad.size == sizeOutputs)
      val outbase = outputs.map(derivActivation) elem_* outGrad
      val inputsAndSpecial = Array(1.0) ++ inputs
      GradInfo(
        inputGradient = (0 until sizeInputs).map { i =>
          (0 until sizeOutputs).map(o => outbase(o) * weights(o * perneuron + i + 1)).reduce(_ + _)
        }.toArray,
        weightGradient = (0 until sizeOutputs).flatMap(o => inputsAndSpecial * outbase(o)).toArray
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
  private def basicLayer(sizeInput: Int, sizeOutput: Int): DeepNN =
    new SummingLayer(sizeInput, sizeOutput) with TanhActivation

  def basicNN(sizes: Int*): DeepNN =
    (sizes.take(sizes.size - 1), sizes.takeRight(sizes.size - 1)).zipped.map(basicLayer(_, _)).reduce(_ | _)

}
