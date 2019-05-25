package net.stoerr.learning.learnalgorithmexplorations.common

import org.scalatest.FunSuite

import scala.collection.immutable.SortedMap
import scala.util.Random

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
    assert(10000 == ranges.valuesIterator.sum)
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

  test("rank combine") {
    val a = new StatisticsWithRanges("a")
    val b = new StatisticsWithRanges("b")
    for (i <- 0 until 10000) a += Random.nextInt(100)
    for (i <- 0 until 10000) b += Random.nextInt(100)
    assert(a.count == a.ranges(30).values.sum)
    a ++= b
    assert(a.count == a.ranges(30).values.sum)
    assert(a.count == 20000)
    println(a)
    assert(a.quantile(0) < 5)
    assert(a.quantile(0.9) < 92)
    assert(a.quantile(0.9) > 88)
    assert(a.quantile(1) > 95)
  }
}
