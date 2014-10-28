package net.stoerr.stocklearning

import scala.collection.immutable.{TreeMap, SortedMap}

/** Constructors for DValue - see there. */
object DValue {
  implicit def apply(value: Double) = new DValue(value, SortedMap.empty)
  def apply(value: Double, name: String) = new DValue(value, TreeMap(name -> 1.0))
}

/**
 * Calculation where the partial derivations from an arbitrary number of variables are automatically calculated as well.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.10.2014
 */
case class DValue(value: Double, derivations: SortedMap[String, Double]) {

  def deriv(name: String): Double = derivations.getOrElse(name, 0)

  private def combineDerivations(other: DValue, func: (Double, Double) => Double) = {
    val entries = (this.derivations.keySet ++ other.derivations.keySet).toSeq.map( key =>
      key -> func(this.deriv(key), other.deriv(key))
    )
    SortedMap(entries: _*)
  }

  def +(other: DValue) : DValue = DValue(value + other.value, combineDerivations(other, _ + _))

}