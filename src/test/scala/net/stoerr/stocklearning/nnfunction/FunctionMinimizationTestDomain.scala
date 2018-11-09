package net.stoerr.stocklearning.nnfunction

import java.lang.Math.cosh

import net.stoerr.stocklearning.common.{DoubleArrayVector, Statistics}
import net.stoerr.stocklearning.common.DoubleArrayVector._

import scala.language.implicitConversions
import scala.util.Random

/** We create a random function that has a minimum at (1,1,1...) for value 1, but which is hard to find with gradient descent. Intended as test for minimum finding algorithms. */
case class FunctionMinimizationTestFunction(dimensions: Int) extends Function[Array[Double], Double] {

  val center: Array[Double] = Array.fill(dimensions)(1.0)

  protected val parameters: Array[(Double, Array[Double])] = (1 to 5 * dimensions / 4 + 2).map(_ =>
    (Math.pow(10, Random.nextDouble() * 2),
      DoubleArrayVector((1 to dimensions).map(_ => Random.nextGaussian())).normalize
    )
  ).toArray

  def apply(v: Array[Double]): Double = {
    val v1 = v - center
    parameters.map(p => p._1 * (cosh(p._2 * v1) - 1)).sum
  }

}


case class FunctionMinimizationTestDomain(dimensions: Int, functionCount: Int = 10) {

  val functions: Seq[FunctionMinimizationTestFunction] = (1 to functionCount).map(_ => FunctionMinimizationTestFunction(dimensions))

  def fitness(approximator: (Array[Double] => Double) => Array[Double]): Double =
    -functions.map(f => f(approximator(f))).sum

  def evaluateStatistics(approximator: (Array[Double] => Double) => Array[Double]): Statistics = {
    val statistics = new Statistics("FunctionMinimizationTestDomain")
    functions.foreach(f => statistics += f(approximator(f)))
    statistics
  }

}
