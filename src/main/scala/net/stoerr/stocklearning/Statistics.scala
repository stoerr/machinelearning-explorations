package net.stoerr.stocklearning

/**
 * Collects statistics for a single parameter.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.11.2014
 */
class Statistics(name: String) {

  var count: Int
  private var sum: Double
  private var sumsquares: Double
  var min: Double = Double.PositiveInfinity
  var max: Double = Double.NegativeInfinity

  def add(x: Double): Unit = {
    count += 1
    sum += x
    sumsquares += x * x
    min = math.min(min, x)
    max = math.max(max, x)
  }

  def mean = sum / count

  def stddev = math.sqrt((sumsquares - sum * sum / count) / (count - 1))

  def toString() = name + " = " + count + " :[ " + min + " , " mean +" +- " + stddev + " , " + max + " ]"

}
