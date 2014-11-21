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

}
