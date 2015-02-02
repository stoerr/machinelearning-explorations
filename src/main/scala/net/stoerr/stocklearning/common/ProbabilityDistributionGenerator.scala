package net.stoerr.stocklearning.common

import scala.util.Random

/**
 * Draws items from a denormalized probability distribution
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.02.2015
 */
class ProbabilityDistributionGenerator[T](probabilities: Traversable[(T, Double)]) {
  require(probabilities.map(_._2).min >= 0)
  val sum = probabilities.map(_._2).sum

  private val start: (T, Double) = (probabilities.head._1, 0)
  val propabilityfunction: Traversable[(T, Double)] = probabilities.scan(start)((t1, t2) => (t2._1, t1._2 + t2._2))

  def draw(): T = propabilityfunction.find(_._2 >= Random.nextDouble() * sum).get._1
}
