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
    val outputs = (0 until sizeOutputs).par.map { o =>
      activation((0 until sizeInputs).map(i => inputs(i) * weights(o * sizeOutputs + i)).reduce(_ + _))
    }.toArray
    (outputs, { outGrad: Array[Double] =>
      val outbase = outGrad elem_* outputs.map(derivActivation)
      GradInfo(
        inputGradient = (0 until sizeInputs).par.map { i =>
          (0 until sizeOutputs).map(o => outbase(o) * weights(o * sizeOutputs + i)).reduce(_ + _)
        }.toArray,
        weightGradient = (for (o <- 0 until sizeOutputs; i <- 0 until sizeInputs) yield outbase(o) * inputs(i)).toArray
      )
    }
      )
  }

}

trait ActivationFunction {
  def activation(sum: Double): Double

  def derivActivation(output: Double): Double
}

trait TanhActivation extends ActivationFunction {
  override def activation(sum: Double): Double = math.tanh(sum)

  override def derivActivation(output: Double): Double = 1 - output * output
}
