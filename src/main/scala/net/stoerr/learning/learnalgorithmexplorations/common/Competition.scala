package net.stoerr.learning.learnalgorithmexplorations.common

import scala.collection.parallel.mutable.ParArray
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

  def keepRatio = 0.7

  private def trainOrCatch(c: COMPETITOR): Option[COMPETITOR] = try {
    Some(train(c))
  } catch {
    case e: Exception => e.printStackTrace(); None
  }

  /** higher is better */
  def eval(c: COMPETITOR): Double

  def compete(rounds: Int, numCompetitors: Int)(implicit ct: ClassTag[COMPETITOR]): COMPETITOR = {
    val numKept = (numCompetitors * keepRatio).round.toInt
    var competitorsOldGeneration: Array[COMPETITOR] = Array.fill(numCompetitors)(makeCompetitor())
    var competitorsNewGeneration: Array[COMPETITOR] = Array()

    def trainAndEval(competitors: Array[COMPETITOR], descr: String): Array[(Double, COMPETITOR)] = {
      val res: ParArray[(Double, COMPETITOR)] = competitors.par.flatMap(trainOrCatch(_)).map(c => (-eval(c), c))
      // println(new Statistics(descr) ++= res.map(-_._1))
      res.seq.toArray
    }

    val endtime = System.currentTimeMillis() + 3600L * 1000 * 10
    for (r <- 0 until rounds if System.currentTimeMillis() < endtime) {
      println("Competition round " + r)
      val og = trainAndEval(competitorsOldGeneration, "round " + r + " og")
      val ng = trainAndEval(competitorsNewGeneration, "round " + r + " ng")
      val sortedCompetitors = (og ++ ng).sortBy(_._1).map(_._2)
      competitorsOldGeneration = sortedCompetitors.take(numKept)
      competitorsNewGeneration = Array.fill(numCompetitors - competitorsOldGeneration.size)(makeCompetitor())
    }
    competitorsOldGeneration(0)
  }

}
