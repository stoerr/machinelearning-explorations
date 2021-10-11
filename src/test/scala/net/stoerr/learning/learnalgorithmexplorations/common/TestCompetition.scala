package net.stoerr.learning.learnalgorithmexplorations.common

import org.scalatest.funsuite.AnyFunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 23.11.2014
 */
class TestCompetition extends AnyFunSuite {

  test("competition for smallest random") {
    val smlrandom = new Competition[Double] {

      override def makeCompetitor(): Double = math.random

      override def eval(c: Double): Double = -math.abs(c - 0.1)

      override def train(c: Double): Double = (c - 0.1) * 0.9 + 0.1
    }

    val res = smlrandom.compete(20, 20)
    println(res)
    assert(math.abs(res - 0.1) < 0.01)
  }

}
