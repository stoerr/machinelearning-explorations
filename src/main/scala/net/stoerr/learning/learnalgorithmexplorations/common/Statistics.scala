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
class Statistics(name: String) {

  var count: Int = 0
  var sum: Double = 0.0
  var sumsquares: Double = 0.0
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

  def ++=(other : Statistics): this.type = {
    count += other.count
    sum += other.sum
    sumsquares += other.sumsquares
    min = math.min(min, other.min)
    max = math.max(max, other.max)
    return this
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

  def stddev = math.sqrt((sumsquares - sum * sum / count) / (count - 1.5))

  override def toString = name + " = " + mean + " +- " + stddev + " [ " + min + " , " + max + " ] : " + count

}

class StatisticsWithRanges(name: String) extends Statistics(name) {
  private val maxbuckets = 200
  private var buckets: immutable.SortedMap[Double, Int] = TreeMap()

  override def +=(x: Double): this.type = {
    super.+=(x)
    buckets = buckets + (x -> (buckets.getOrElse(x, 0) + 1))
    if (buckets.size >= maxbuckets) buckets = ranges(maxbuckets / 2)
    this
  }

  override def ++=(other : Statistics): this.type = {
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

  def quantile(q: Double): Double = {
    if (q < 0) return buckets.head._1
    val quantileCount = math.round(q * count)
    var haveCount: Long = 0
    for (entry <- buckets) {
      haveCount += entry._2
      if (haveCount > quantileCount) return entry._1
    }
    return buckets.last._1
  }

  override def toString = super.toString + "\n" + ranges(10)

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
