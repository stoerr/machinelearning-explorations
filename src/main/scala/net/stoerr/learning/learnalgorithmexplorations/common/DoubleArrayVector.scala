package net.stoerr.learning.learnalgorithmexplorations.common

import scala.collection.GenTraversableOnce
import scala.language.implicitConversions
import scala.util.Random

object DoubleArrayVector {
  type Vec = Array[Double]

  implicit def doubleArrayVector(v: Vec): DoubleArrayVector = new DoubleArrayVector(v)

  /** step for numerical calcuations */
  val eps = 1e-6

  def derivation(f: Double => Double)(x: Double): Double = (-f(x + 2 * eps) + 8 * f(x + eps) - 8 * f(x - eps) + f(x - 2 * eps)) / (12 * eps)

  def gradient(f: Vec => Double)(x: Vec): Vec = x.indices.map { i =>
    val fprojected = x.baseFunction(i) andThen f
    derivation(fprojected)(0)
  }.toArray

  def apply(v: GenTraversableOnce[Double]): DoubleArrayVector = doubleArrayVector(v.toArray)

  def randomVector(dim: Int): Vec = 0.until(dim).map(_ => Random.nextGaussian()).toArray

}

/**
  * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
  * @since 12.11.2014
  */
final class DoubleArrayVector(val self: DoubleArrayVector.Vec) {

  import DoubleArrayVector._

  def abs: Double = math.sqrt(this * self)

  def elem_abs: Vec = self.map(math.abs)

  def +(other: Vec): Vec = (self, other).zipped.map(_ + _)

  def -(other: Vec): Vec = (self, other).zipped.map(_ - _)

  /** scalar product */
  def *(other: Vec): Double = (self, other).zipped.map(_ * _).sum

  def *(other: Double): Vec = self.map(_ * other)

  /** elementwise product */
  def elem_*(other: Vec): Vec = (self, other).zipped.map(_ * _)

  def /(other: Double): Vec = self.map(_ / other)

  def signum: Vec = self.map(math.signum)

  /** function arg => self + Array(0,...,arg, ...,0) , arg is put in n-th place. */
  def baseFunction(n: Int): Double => Vec = { arg: Double =>
    val basePlusArg = self.clone()
    basePlusArg(n) += arg
    basePlusArg
  }

  def directionalFunction(func: Vec => Double, dx: Vec): Double => Double = (v: Double) => func(self + dx * v)

  def toRep: String = "Array(" + self.mkString(", ") + ")"

  def normalize: Vec = this / abs

  /** Random vector orthogonal to this but with the same length. */
  def randomOrthogonalVector(): Vec = {
    var random = randomVector(self.length).normalize
    val normalizedself = self.normalize
    random = (random - normalizedself * (normalizedself * random)).normalize
    random * (self.abs)
  }

}
