package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common.Statistics
import net.stoerr.stocklearning.java.DoubleArrayOps
import net.stoerr.stocklearning.nnfunction.Example.ValueWithGradient

import scala.collection.GenTraversableOnce

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
class NNasFunction(val inputSize: Int, val hiddenSize: Int, val outputSize: Int) {

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

  def toString(w: Array[Double]): String =
    (0 until hiddenSize).map(i =>
      (0 until inputSize).map(j => w(ind_w1(i, j))).mkString(" ") + " + " + w(ind_offset1(i))
        + " => " + (0 until outputSize).map(k => w(ind_w2(k, i))).mkString(" ")
    ).mkString("\n") + "\n++ " + (0 until outputSize).map(k => w(ind_offset2(k))).mkString(" ")

  private class Calculation(example: Example, weights: Array[Double]) {
    val hiddenOut: Array[Double] = Array.ofDim(hiddenSize)
    for (i <- 0.until(hiddenSize)) hiddenOut(i) = DoubleArrayOps.dotProductAndTanh(inputSize, example.inputs, 0, weights, ind_w1(i, 0))
    val out: Array[Double] = Array.ofDim(outputSize)
    for (i <- 0.until(outputSize)) out(i) = DoubleArrayOps.dotProductAndTanh(hiddenSize, hiddenOut, 0, weights, ind_w2(i, 0))
  }

  private class DerivCalculation(example: Example, weights: Array[Double]) extends Calculation(example, weights) {
    private def sqr(x: Double) = x * x

    val (gain, gainOut2Gradient) = example.gainWithGradient(out)
    val gradient: Array[Double] = Array.ofDim(dimension)
    val hiddenDivSum: Array[Double] = Array.ofDim(hiddenSize) // d(gain)/d(hiddenOut(_))
    for ((o2i, i) <- out.zipWithIndex) {
      val o2id = (1 - o2i * o2i) * gainOut2Gradient(i) // d(gain)/d(o2_i)
      //      for (j <- 0 until hiddenSize) {
      //        val ind_w2_ij: Int = ind_w2(i, j)
      //        gradient(ind_w2_ij) = o2id * hiddenOut(j)
      //        hiddenDivSum(j) += o2id * weights(ind_w2_ij)
      //      }
      DoubleArrayOps.assignMultiplied(hiddenSize, hiddenOut, 0, gradient, ind_w2(i, 0), o2id)
      DoubleArrayOps.addMultiplied(hiddenSize, weights, ind_w2(i, 0), hiddenDivSum, 0, o2id)
      gradient(ind_offset2(i)) = o2id
    }
    for ((o1j, j) <- hiddenOut.zipWithIndex) {
      val o1jd = hiddenDivSum(j) * (1 - sqr(hiddenOut(j)))
      //      for (k <- 0 until inputSize) {
      //        gradient(ind_w1(j, k)) = o1jd * example.inputs(k)
      //      }
      DoubleArrayOps.assignMultiplied(inputSize, example.inputs, 0, gradient, ind_w1(j, 0), o1jd)
      gradient(ind_offset1(j)) = o1jd
    }
    val result = (gain, gradient)
  }

  def gain(weights: Array[Double], example: Example) = example.gain(new Calculation(example, weights).out)

  def weightFunction(example: Example): (Array[Double] => Double) = weights => gain(weights, example)

  def joinedWeightFunction(examples: Seq[Example]): (Array[Double] => Double) =
    examples.map(weightFunction).reduceLeft((f1, f2) => weights => f1(weights) + f2(weights)).andThen(_ / examples.size)

  def weightFunctionWithGradient(example: Example): (Array[Double] => ValueWithGradient) = weights => new DerivCalculation(example, weights).result

  def joinedWeightFunctionWithGradient(examples: Seq[Example]): (Array[Double] => ValueWithGradient) =
    examples.map(weightFunctionWithGradient).reduceLeft { (f1, f2) =>
      weights => {
        val (f1g, f1d) = f1(weights)
        val (f2g, f2d) = f2(weights)
        (f1g + f2g, f1d + f2d)
      }
    }.andThen { case (v, g) => (v / examples.size, g / examples.size)}

  def statistics(name: String, weights: Array[Double], examples: GenTraversableOnce[Example]): Statistics = {
    val stats = new Statistics(name)
    examples.foreach(ex => stats += gain(weights, ex))
    stats
  }

}
