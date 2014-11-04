package net.stoerr.stocklearning

import scala.collection.immutable.TreeMap

/**
 * Main program for search for option trade strategies.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.11.2014
 */
object RunOptionStrategySearch {

  def main(args: Array[String]) {
    val ex = new OptionStrategyExample(5, -1)
  }

}
