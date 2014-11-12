package net.stoerr.stocklearning.common

object DoubleArrayVector {
  implicit def doubleArrayVector(v: Array[Double]) = new DoubleArrayVector(v)
}

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 12.11.2014
 */
final class DoubleArrayVector(val self: Array[Double]) {

  def abs = math.sqrt(this * self)

  def +(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ + _)

  def -(other: Array[Double]): Array[Double] = (self, other).zipped.map(_ - _)

  /** scalar product */
  def *(other: Array[Double]): Double = (self, other).zipped.map(_ * _).reduce(_ + _)

  def *(other: Double): Array[Double] = self.map(_ * other)

}
