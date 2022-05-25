package net.stoerr.learning.learnalgorithmexplorations.common

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.{GenTraversableOnce, immutable}

/**
 * Collects statistics for a single parameter.
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.11.2014
 */
class Statistics(val name: String, val filterNan: Boolean = true) {

  /** Count of all items - if filterNan=true these are only counted in nanCount. */
  var count: Int = 0
  var sum: Double = 0.0
  var sumsquares: Double = 0.0
  var min: Double = Double.PositiveInfinity
  var max: Double = Double.NegativeInfinity
  var nanCount: Int = 0

  def +=(x: Double): this.type = {
    if (x.isNaN || x.isInfinite) {
      nanCount += 1
    } else {
      count += 1
      sum += x
      sumsquares += x * x
      min = math.min(min, x)
      max = math.max(max, x)
    }
    this
  }

  def ++=(values: GenTraversableOnce[Double]): this.type = {
    values.foreach(this += _)
    this
  }

  def ++=(other: Statistics): this.type = {
    require(filterNan == other.filterNan)
    count += other.count
    sum += other.sum
    sumsquares += other.sumsquares
    min = math.min(min, other.min)
    max = math.max(max, other.max)
    nanCount += other.nanCount
    return this
  }

  def *(fact: Double): Statistics = {
    val res = new Statistics(name, filterNan)
    res.count = count
    res.sum = fact * sum
    res.sumsquares = fact * fact * sumsquares
    res.min = math.max(fact * min, fact * max)
    res.max = math.min(fact * min, fact * max)
    res.nanCount = nanCount
    res
  }

  override def toString = name + " = " + mean + " +- " + stddev + " [ " + min + " , " + max + " ] : " +
    count + (if (filterNan && nanCount > 0) " +NaN " + (nanpart * 100).toFloat + "%" else "")

  def mean: Double = sum / count

  def stddev: Double = math.sqrt((sumsquares - sum * sum / count) / (count - 1.5))

  /** Part of the numbers that have been NaN or Infinity. */
  def nanpart: Double = {
    require(filterNan, "NaN counting not enabled for " + name)
    nanCount * 1.0 / (count + nanCount)
  }

}

class StatisticsWithRanges(name: String, filterNan: Boolean = true, maxbuckets:Int = 200) extends Statistics(name, filterNan) {

  /** Each bucket maps the center of the bucket (arithmetic mean of all entries that went into the bucket)
   * to the number of items in that bucket. That makes it harder to calculate quantiles than if we would store
   * the right border of the bucket instead, but the buckets are more adaptive. */
  private var buckets: immutable.SortedMap[Double, Int] = TreeMap()

  override def +=(x: Double): this.type = {
    super.+=(x)
    if (!filterNan || (!x.isNaN && !x.isInfinite)) {
      buckets = buckets + (x -> (buckets.getOrElse(x, 0) + 1))
      if (buckets.size >= maxbuckets) buckets = ranges(maxbuckets / 2)
    }
    this
  }

  override def ++=(other: Statistics): this.type = {
    val o = other.asInstanceOf[StatisticsWithRanges]
    super.++=(other)
    o.buckets.foreach(b => buckets = buckets + (b._1 -> (buckets.getOrElse(b._1, 0) + b._2)))
    if (buckets.size >= maxbuckets) buckets = ranges(maxbuckets / 2)
    return this
  }

  def ranges(bucketcount: Int): immutable.SortedMap[Double, Int] = {
    def joinBuckets(bucket1: (Double, Int), bucket2: (Double, Int)) =
      if (0 == bucket1._2) bucket2
      else ((bucket1._1 * bucket1._2 + bucket2._1 * bucket2._2) / (bucket1._2 + bucket2._2), bucket1._2 + bucket2._2)

    val bucketstep: Double = count / bucketcount
    val emptybucket = (Double.NegativeInfinity, 0)
    var nextbucketboundary = bucketstep
    var currentbucket = emptybucket
    var currentelementcount = 0 // up to currentbucket
    val newbuckets = buckets.flatMap { bucket =>
      if (currentelementcount >= nextbucketboundary) {
        val b = currentbucket
        currentbucket = bucket
        nextbucketboundary += bucketstep
        Some(b)
      } else {
        currentbucket = joinBuckets(currentbucket, bucket)
        currentelementcount += bucket._2
        None
      }
    }
    newbuckets + currentbucket
  }

  def quantile(q: Double): Double = {
    if (count <= 0) return Double.NaN
    if (q <= 0) return min
    if (q >= 1) return max
    val quantileCount = math.round(q * count)
    if (quantileCount <= buckets.head._2 / 2) {
      return interpolate(0, min, buckets.head._2 / 2, buckets.head._1, quantileCount)
    }
    if (quantileCount >= count - buckets.last._2 / 2) {
      return interpolate(count - buckets.last._2 / 2, buckets.last._1, count, max, quantileCount)
    }
    val it = buckets.iterator.buffered
    var countbeforelast = 0
    var lastbucket: (Double, Int) = it.next()
    while (countbeforelast + lastbucket._2 + it.head._2 / 2 < quantileCount) {
      if (lastbucket != null) countbeforelast += lastbucket._2
      lastbucket = it.next()
    }
    // now the quantile lies between lastbucket and it.head .
    interpolate(countbeforelast + lastbucket._2 / 2.0, lastbucket._1,
      countbeforelast + lastbucket._2 + it.head._2 / 2.0, it.head._1, quantileCount)
  }

  protected def interpolate(x1: Double, y1: Double, x2: Double, y2: Double, x: Double): Double =
    y1 + (y2 - y1) * (x - x1) / (x2 - x1)

  def median = quantile(0.5)

  override def toString = super.toString + "\n" +
    "[" + ranges(10).map(e => e._2 + "*" + e._1.toFloat).mkString(", ") + "]"

}

class XYStatistics(name: String) {
  val xstats = new Statistics(name + " x")
  val ystats = new Statistics(name + " y")
  private var sumXY = 0.0

  def +=(xy: (Double, Double)): this.type = {
    xstats += xy._1
    ystats += xy._2
    sumXY += xy._1 * xy._2
    this
  }

  def ++=(values: GenTraversableOnce[(Double, Double)]): this.type = {
    values.foreach(this += _)
    this
  }

  def correlationcoefficient: Double = (xstats.count * sumXY - xstats.sum * ystats.sum) /
    math.sqrt(xstats.count * xstats.sumsquares - xstats.sum * xstats.sum) /
    math.sqrt(ystats.count * ystats.sumsquares - ystats.sum * ystats.sum)

  /** Linear regression: slope * x + intercept = y. */
  def slope: Double =
    (xstats.count * sumXY - xstats.sum * ystats.sum) / (xstats.count * xstats.sumsquares - xstats.sum * xstats.sum)

  def intercept: Double = ystats.mean - xstats.mean * slope

  override def toString = name + ": " + correlationcoefficient + "\n" + xstats + "\n" + ystats + "\n"
}

class XYStatisticsWithRankCorrelation(name: String) extends XYStatistics(name) {

  val xvalues = new ArrayBuffer[Double]()
  val yvalues = new ArrayBuffer[Double]()

  override def +=(xy: (Double, Double)): this.type = {
    super.+=(xy)
    xvalues += xy._1
    yvalues += xy._2
    this
  }

  def rankCorrelationCoefficient: Double = {
    val xvalueRanks = xvalues.zipWithIndex.sortBy(_._1).toIterator.map(_._2.asInstanceOf[Double])
    val yvalueRanks = yvalues.zipWithIndex.sortBy(_._1).toIterator.map(_._2.asInstanceOf[Double])
    val rankstats = new XYStatistics("")
    rankstats ++= xvalueRanks.zip(yvalueRanks)
    rankstats.correlationcoefficient
  }

  override def toString = super.toString + "\nrankCorrelation = " + rankCorrelationCoefficient
}
