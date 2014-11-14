package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.java.DoubleArrayOps._
import net.stoerr.stocklearning.nnfunction.Example.ValueWithGradient

trait DoubleFunctionWithGradient extends (Array[Double] => Double) {
  def gradient(arg: Array[Double]): Array[Double]

  def applyWithGradient(arg: Array[Double]): ValueWithGradient = (apply(arg), gradient(arg))
}

/**
 * Makes a function R to the n -> R from a neural network with examples,
 * such that we can use more sophisticated algorithms for minimum search <br/>
 * The function is a function of the weight vector:
 * hiddenSize times inputSize weights, outputSize times hiddenSize weights,
 * hiddenSize offsets, outputSize offsets. <br/>
 * TODO: offsets???
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 11.11.2014
 */
class NNasFunction(inputSize: Int, hiddenSize: Int, outputSize: Int) {

  /** Dimension of the weight vector.
    * w1_jk (in_k -> o1_j) : (inputSize + 1)*j + k
    * offset1_j (-> o1_j) : (inputSize + 1)*j + inputSize
    * w2_ij (o1_j -> o2_i) : (inputSize + 1) * hiddenSize + (hiddenSize + 1) * i + j
    * offset2_i (-> o2_i) : (inputSize + 1) * hiddenSize + (hiddenSize + 1) * i + hiddenSize
    */
  val dimension = (inputSize + 1) * hiddenSize + (hiddenSize + 1) * outputSize

  private def ind_w1(j: Int, k: Int) = (inputSize + 1) * j + k

  private def ind_offset1(j: Int) = ind_w1(j, inputSize)

  private def ind_w2(i: Int, j: Int) = (inputSize + 1) * hiddenSize + (hiddenSize + 1) * i + j

  private def ind_offset2(i: Int) = ind_w2(i, hiddenSize)

  private class Calculation(example: Example, weights: Array[Double]) {
    val hiddenOut: Array[Double] = Array.ofDim(hiddenSize)
    for (i <- 0.until(hiddenSize)) hiddenOut(i) = dotProductAndTanh(inputSize, example.inputs, 0, weights, ind_w1(i, 0))
    val out: Array[Double] = Array.ofDim(outputSize)
    for (i <- 0.until(outputSize)) out(i) = dotProductAndTanh(hiddenSize, hiddenOut, 0, weights, ind_w2(i, 0))
  }

  private class DerivCalculation(example: Example, weights: Array[Double]) extends Calculation(example, weights) {
    val (gain, gainOut2Gradient) = example.gainWithGradient(out)
    val gradient: Array[Double] = Array.ofDim(dimension)
    for ((o2i, i) <- out.zipWithIndex) {
      val o2id = (1 - o2i * o2i) * gainOut2Gradient(i) // d(gain)/d(o2_i)
      for ((o1, j) <- hiddenOut.zipWithIndex) {
        // gradient((inputSize + 1) * hiddenSize + (hiddenSize + 1) * i))
      }
    }
    val result = (gain, gradient)
  }

  def weightFunction(example: Example): (Array[Double] => Double) = weights => example.gain(new Calculation(example, weights).out)

  def joinedWeightFunction(examples: Seq[Example]): (Array[Double] => Double) =
    examples.map(weightFunction).reduceLeft((f1, f2) => (weights => f1(weights) + f2(weights)))

  def weightFunctionWithGradient(example: Example): (Array[Double] => ValueWithGradient) = weights => new DerivCalculation(example, weights).result

  def joinedWeightFunctionWithGradient(examples: Seq[Example]): (Array[Double] => ValueWithGradient) =
    examples.map(weightFunctionWithGradient).reduceLeft { (f1, f2) =>
      weights => {
        val (f1g, f1d) = f1(weights)
        val (f2g, f2d) = f2(weights)
        (f1g + f2g, f1d + f2d)
      }
    }


}
