package net.stoerr.stocklearning.common

import scala.language.implicitConversions

object DoubleArrayVector {
  implicit def doubleArrayVector(v: Array[Double]) = new DoubleArrayVector(v)
}

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 12.11.2014
 */
final class DoubleArrayVector(val self: Array[Double]) {

  import DoubleArrayVector._

  def abs = math.sqrt(this * self)

  def +(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ + _)

  def -(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ - _)

  /** scalar product */
  def *(other: Array[Double]): Double = (self, other).zipped.map(_ * _).reduce(_ + _)

  def *(other: Double): Array[Double] = self.map(_ * other)

  def /(other: Double): Array[Double] = self.map(_ / other)

  /** func(self + Array(0,...,arg, ...,0)) - func(self) , arg is put in n-th place. */
  def projectFunction(func: Array[Double] => Double, n: Int) = { arg: Double =>
    val basePlusArg = self.clone()
    basePlusArg(n) += arg
    func(basePlusArg)
  }

  def directionalFunction(func: Array[Double] => Double, x: Array[Double], dx: Array[Double])(v: Double): Double
  = func(x + dx * v)

}
