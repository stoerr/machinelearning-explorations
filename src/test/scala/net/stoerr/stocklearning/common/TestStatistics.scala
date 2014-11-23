package net.stoerr.stocklearning.common

import org.scalatest.FunSuite

import scala.collection.immutable.SortedMap

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.11.2014
 */
class TestStatistics extends FunSuite {

  test("buckets") {
    val stats = new StatisticsWithRanges("random")
    for (i <- 0 until 10000) stats += math.random
    println(stats)
    val ranges: SortedMap[Double, Int] = stats.ranges(10)
    assert(ranges.size == 10)
    assert(10000 == ranges.valuesIterator.reduce(_ + _))
  }

  test("xystats - no correlation") {
    val xystats = new XYStatisticsWithRankCorrelation("xystats")
    for (i <- 0 until 10000) xystats += (math.random, math.random)
    println(xystats)
    assert (math.abs(xystats.correlationcoefficient) < 0.1)
    assert (math.abs(xystats.rankCorrelationCoefficient) < 0.1)
  }

  test("xystats - full correlation") {
    val xystats = new XYStatistics("xystats")
    for (i <- 0 until 10000) {
      val rnd = math.random
      xystats += (rnd, rnd)
    }
    println(xystats)
    assert (math.abs(xystats.correlationcoefficient) > 0.999999)
    assert (math.abs(xystats.correlationcoefficient) < 1.000001)
  }

  test("rankstats") {
    val xystats = new XYStatisticsWithRankCorrelation("xystats")
    for (i <- 0 until 10000) {
      val rnd = math.random
      xystats += (rnd, math.sqrt(rnd))
    }
    println(xystats)
    assert (math.abs(xystats.rankCorrelationCoefficient) > 0.999999)
    assert (math.abs(xystats.rankCorrelationCoefficient) < 1.000001)
  }
}
