package net.stoerr.stocklearning.nnfunction

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
 * hiddenSize times inputSize weights, outputSize times hiddenSize weights
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 11.11.2014
 */
class NNasFunction(inputSize: Int, hiddenSize: Int, outputSize: Int) {

  /** Dimension of the weight vector */
  val dimension = hiddenSize * inputSize + outputSize * hiddenSize

  private class Calculation(example: Example, weights: Array[Double]) {
    val hiddenOut: Array[Double] = Array.ofDim(hiddenSize)
    for (i <- 0.until(hiddenSize)) hiddenOut(i) = dotProductAndTanh(inputSize, example.inputs, 0, weights, inputSize * i)
    val out: Array[Double] = Array.ofDim(outputSize)
    for (i <- 0.until(outputSize)) out(i) = dotProductAndTanh(hiddenSize, hiddenOut, 0, weights, inputSize * hiddenSize + hiddenSize * i)
  }

  private class DerivCalculation(example: Example, weights: Array[Double]) extends Calculation(example, weights) {
    val (gain, gainOut2Gradient) = example.gainWithGradient(out)
    val gradient: Array[Double] = Array.ofDim(dimension)
  }

  private def evaluateNN(inputs: Array[Double], weights: Array[Double]): Array[Double] = new Calculation(inputs, weights).out

  def weightFunction(example: Example): (Array[Double] => Double) = weights => example.gain(evaluateNN(example.inputs, weights))

  def joinedWeightFunction(examples: Seq[Example]): (Array[Double] => Double) =
    examples.map(weightFunction).reduceLeft((f1, f2) => (weights => f1(weights) + f2(weights)))

  def weightFunctionWithGradient(example: Example): (Array[Double] => Double) = weights => example.gain(evaluateNN(example.inputs, weights))

  def joinedWeightFunctionWithGradient(examples: Seq[Example]): (Array[Double] => Double) =
    examples.map(weightFunction).reduceLeft((f1, f2) => (weights => f1(weights) + f2(weights)))


}
