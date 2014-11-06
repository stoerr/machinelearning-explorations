package net.stoerr.stocklearning

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

  def +=(x: Double): Unit = {
    count += 1
    sum += x
    sumsquares += x * x
    min = math.min(min, x)
    max = math.max(max, x)
  }

  def mean = sum / count

  def stddev = math.sqrt((sumsquares - sum * sum / count) / (count - 1))

  override def toString() = name + " = " + mean + " +- " + stddev + " [ " + min + " , " + max + " ] : " + count

}
