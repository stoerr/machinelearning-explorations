package net.stoerr.learning.learnalgorithmexplorations.deepnn

import net.stoerr.learning.learnalgorithmexplorations.nnfunction.Example
import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector._

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

  /** R**sizeInputs x R**sizeWeights -> R */
  def f(example: Example)(weights: Array[Double]): Double = fgrad(example)(weights)._1

  def fgradCombined(examples: Seq[Example])(weights: Array[Double]): (Double, Array[Double]) = {
    examples.par.map(fgrad(_)(weights)).reduce((x, y) => (x._1 + y._1, x._2 + y._2))
  }

  def fCombined(examples: Seq[Example])(weights: Array[Double]): Double = fgradCombined(examples)(weights)._1

  def layers: List[DeepNNLayer]

  override def toString: String = s"$sizeInputs($sizeWeights)$sizeOutputs"
}

object DeepNN {

  def join(m: DeepNN, n: DeepNN): DeepNN = new DeepNN {
    assert(m.sizeOutputs == n.sizeInputs)

    override val sizeInputs: Int = m.sizeInputs
    override val sizeWeights: Int = m.sizeWeights + n.sizeWeights
    override val sizeOutputs: Int = n.sizeOutputs

    override def fg(inputs: Array[Double])(weights: Array[Double]): (Array[Double], Array[Double] => GradInfo) = {
      assert(m.sizeInputs == inputs.size, s"${m.sizeInputs} == ${inputs.size}")
      assert(sizeWeights == weights.size, s"$sizeWeights == ${weights.size}")
      val (my, mg) = m.fg(inputs)(weights.slice(0, m.sizeWeights))
      val (ny, ng) = n.fg(my)(weights.slice(m.sizeWeights, sizeWeights))
      (ny, outputGradient => {
        val nginfo = ng(outputGradient)
        val mginfo = mg(nginfo.inputGradient)
        GradInfo(inputGradient = mginfo.inputGradient, weightGradient = mginfo.weightGradient ++ nginfo.weightGradient)
      })
    }

    override def layers: List[DeepNNLayer] = m.layers ++ n.layers

    override def toString() = m.toString + "|" + n.toString
  }

}
