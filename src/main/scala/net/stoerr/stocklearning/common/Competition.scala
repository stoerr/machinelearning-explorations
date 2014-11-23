package net.stoerr.stocklearning.common

import scala.collection.immutable.TreeMap
import scala.reflect.ClassTag

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 22.11.2014
 */
trait Competition[COMPETITOR] {

  def makeCompetitor(): COMPETITOR

  def train(c: COMPETITOR): COMPETITOR

  def freshRatio = 0.33

  /** higher is better */
  def eval(c: COMPETITOR): Double

  def compete(rounds: Int, numCompetitors: Int)(implicit ct: ClassTag[COMPETITOR]): COMPETITOR = {
    var competitors = Array.fill(numCompetitors)(makeCompetitor())
    val numfresh = (numCompetitors * freshRatio).round.toInt
    for (r <- 0 until rounds) {
      competitors = competitors map (train(_))
      val evaluated = competitors.map(c => (-eval(c), c)).sortBy(_._1)
      println("Competition round " + r)
      competitors = evaluated.take(numCompetitors - numfresh).map(_._2) ++ Array.fill(numfresh)(makeCompetitor())
    }
    competitors(0)
  }

}
