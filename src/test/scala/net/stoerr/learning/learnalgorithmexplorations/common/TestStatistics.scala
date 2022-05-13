package net.stoerr.learning.learnalgorithmexplorations.common

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedMap
import scala.util.Random

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.11.2014
 */
class TestStatistics extends AnyFunSuite {

  test("buckets") {
    val stats = new StatisticsWithRanges("random")
    for (i <- 0 until 10000) stats += math.random
    println(stats)
    val ranges: SortedMap[Double, Int] = stats.ranges(10)
    assert(ranges.size == 10)
    assert(10000 == ranges.valuesIterator.sum)
  }

  test("nan etc") {
    val stats = new StatisticsWithRanges("sum")
    for (i <- 0 until 1001) stats += i
    println(stats)
    val origmean = stats.mean
    stats += Double.NaN
    stats += Double.NegativeInfinity
    stats += Double.PositiveInfinity
    println(stats)
    assert(stats.nanCount == 3)
    assert(stats.count == 1001)
    assert(stats.mean == origmean)
    val bucketmean = stats.ranges(10).toSeq.map(e => e._1 * e._2).sum / stats.count
    assert(Math.abs(bucketmean - stats.mean) < 0.0001, bucketmean)
    assert( Math.abs(stats.nanpart - 3.0/1001) < 0.00001, stats.nanpart)
    assert(Math.abs(stats.quantile(0.0005) - 0.5) < 0.00001, stats.quantile(0.0005))
    assert(Math.abs(stats.quantile(0.9995) - 999.5) < 0.00001, stats.quantile(0.9995))
    assert(Math.abs(stats.quantile(0.1) - 100) < 1, stats.quantile(0.1))
    assert(Math.abs(stats.quantile(0.9985) - 998.5) < 0.00001, stats.quantile(0.9985))
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

  test("xystats - slope") {
    val xystats = new XYStatistics("xystats")
    for (i <- 0 until 10000) {
      val x = math.random - 0.5
      xystats += (x, 2 * x + 1)
    }
    println(xystats)
    assert (math.abs(xystats.slope - 2) < 0.00001)
    assert (math.abs(xystats.intercept - 1) < 0.00001)
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
