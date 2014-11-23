package net.stoerr.stocklearning.common

import scala.reflect.ClassTag

/**
 * Run a competitive training of many COMPETITORs , periodically removing the worst evaluated
 * competitors and introduce freshRatio new ones.
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
    val numfresh = (numCompetitors * freshRatio).round.toInt
    var competitorsOldGeneration: Array[COMPETITOR] = Array.fill(numCompetitors)(makeCompetitor())
    var competitorsNewGeneration: Array[COMPETITOR] = Array()

    def trainAndEval(competitors: Array[COMPETITOR], descr: String): Array[(Double, COMPETITOR)] = {
      val res = competitors.par map train map (c => (-eval(c), c))
      println(new Statistics(descr) ++= res.map(-_._1))
      res.toArray
    }

    for (r <- 0 until rounds) {
      println("Competition round " + r)
      val og = trainAndEval(competitorsOldGeneration, "round " + r + " og")
      val ng = trainAndEval(competitorsNewGeneration, "round " + r + " ng")
      val sortedCompetitors = (og ++ ng).sortBy(_._1).map(_._2)
      competitorsOldGeneration = sortedCompetitors.take(numCompetitors - numfresh)
      competitorsNewGeneration = Array.fill(numCompetitors - competitorsOldGeneration.size)(makeCompetitor())
    }
    competitorsOldGeneration(0)
  }

}
