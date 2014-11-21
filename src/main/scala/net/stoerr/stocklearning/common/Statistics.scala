package net.stoerr.stocklearning.common

import scala.collection.immutable.TreeMap
import scala.collection.{GenTraversableOnce, immutable}

/**
 * Collects statistics for a single parameter.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.11.2014
 */
class Statistics(name: String) {

  var count: Int = 0
  private var sum: Double = 0.0
  private var sumsquares: Double = 0.0
  var min: Double = Double.PositiveInfinity
  var max: Double = Double.NegativeInfinity

  def +=(x: Double): this.type = {
    count += 1
    sum += x
    sumsquares += x * x
    min = math.min(min, x)
    max = math.max(max, x)
    this
  }

  def ++=(values: GenTraversableOnce[Double]): this.type = {
    values.foreach(this += _)
    this
  }

  def *(fact: Double): Statistics = {
    val res = new Statistics(name)
    res.count = count
    res.sum = fact * sum
    res.sumsquares = fact * fact * sumsquares
    res.min = math.max(fact * min, fact * max)
    res.max = math.min(fact * min, fact * max)
    res
  }

  def mean = sum / count

  def stddev = math.sqrt((sumsquares - sum * sum / count) / (count - 1))

  override def toString = name + " = " + mean + " +- " + stddev + " [ " + min + " , " + max + " ] : " + count

}

class StatisticsWithRanges(name: String) extends Statistics(name) {
  private val maxbuckets = 100
  private var buckets: immutable.SortedMap[Double, Int] = TreeMap()

  override def +=(x: Double): this.type = {
    super.+=(x)
    buckets = buckets + (x -> (buckets.getOrElse(x, 0) + 1))
    if (buckets.size >= maxbuckets) buckets = ranges(maxbuckets / 2)
    this
  }

  def ranges(bucketcount: Int): immutable.SortedMap[Double, Int] = {
    def joinBuckets(bucket1: (Double, Int), bucket2: (Double, Int)) =
      if (0 == bucket1._2) bucket2
      else ((bucket1._1 * bucket1._2 + bucket2._1 * bucket2._2) / (bucket1._2 + bucket2._2), bucket1._2 + bucket2._2)
    val elementcount = buckets.valuesIterator.reduce(_ + _)
    val bucketstep: Double = elementcount / bucketcount
    val emptybucket = (Double.NegativeInfinity, 0)
    var currentelementcount = 0
    var nextbucketboundary = bucketstep
    var currentbucket = emptybucket
    val newbuckets = buckets.flatMap { bucket =>
      currentelementcount += bucket._2
      if (currentelementcount > nextbucketboundary) {
        val b = currentbucket
        currentbucket = bucket
        nextbucketboundary += bucketstep
        Some(b)
      } else {
        currentbucket = joinBuckets(currentbucket, bucket)
        None
      }
    }
    newbuckets + currentbucket
  }

  override def toString = super.toString + "\n" + ranges(10)

}
