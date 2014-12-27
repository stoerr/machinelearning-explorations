package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.nnfunction.Example

case class GradInfo(inputGradient: Array[Double], weightGradient: Array[Double])

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 22.12.2014
 */
trait DeepNN {

  val sizeInputs: Int
  val sizeWeights: Int
  val sizeOutputs: Int

  /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs */
  def f(inputs: Array[Double])(weights: Array[Double]): Array[Double] = fg(inputs)(weights)._1

  /** R**sizeInputs x R**sizeWeights -> R**sizeOutputs , R**sizeOutputs => GradInfo */
  def fg(inputs: Array[Double])(weights: Array[Double]): (Array[Double], Array[Double] => GradInfo)

  def |(o: DeepNN): DeepNN = DeepNN.join(this, o)

  /** R**sizeInputs x R**sizeWeights -> (R, R**sizeWeights) */
  def fgrad(example: Example)(weights: Array[Double]): (Double, Array[Double]) = {
    val (outputs, gradToGradinfo) = fg(example.inputs)(weights)
    val (result, outputgrad) = example.gainWithGradient(outputs)
    val ginfo = gradToGradinfo(outputgrad)
    (result, ginfo.weightGradient)
  }

  def fgradCombined(examples: Seq[Example])(weights: Array[Double]): (Double, Array[Double]) = {
    examples.par.map(fgrad(_)(weights)).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
  }

  def fCombined(examples: Seq[Example])(weights: Array[Double]): Double = fgradCombined(examples)(weights)._1
}

object DeepNN {

  def join(m: DeepNN, n: DeepNN): DeepNN = new DeepNN {
    assert(m.sizeOutputs == n.sizeInputs)

    override val sizeInputs: Int = m.sizeInputs
    override val sizeWeights: Int = m.sizeWeights + n.sizeWeights
    override val sizeOutputs: Int = n.sizeOutputs

    override def fg(inputs: Array[Double])(weights: Array[Double]): (Array[Double], Array[Double] => GradInfo) = {
      assert(m.sizeInputs == inputs.size)
      assert(sizeWeights == weights.size)
      val (my, mg) = m.fg(inputs)(weights.slice(0, m.sizeWeights))
      val (ny, ng) = n.fg(my)(weights.slice(m.sizeWeights, sizeWeights))
      (ny, outputGradient => {
        val nginfo = ng(outputGradient)
        val mginfo = mg(nginfo.inputGradient)
        GradInfo(inputGradient = mginfo.inputGradient, weightGradient = mginfo.weightGradient ++ nginfo.weightGradient)
      })
    }
  }

}
