package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.Timer._
import net.stoerr.stocklearning.common._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 19.11.2014
 */
object RunOptionStrategySearchWithGradientDescent extends App with OptionStrategyExampleSet {

  val nn = new NNasFunction(modelExample.inputs.length, intermediateLayerSize, modelExample.p.length)

  val f = nn.joinedWeightFunction(learnExamples)
  val fgrad = nn.joinedWeightFunctionWithGradient(learnExamples)

  timing("learning") {
    val weights = (0 until nn.dimension).map(_ => math.random - 0.5).toArray

    val learnMaxgain = new Statistics("learnMaxGain")
    learnMaxgain ++= learnExamples.map(_.theoreticalMaximumGain)
    val evalMaxgain = new Statistics("evalMaxGain")
    evalMaxgain ++= evalExamples.map(_.theoreticalMaximumGain)

    val (nweights, lastgain, lastchange) = new GradientDescentWithWithMinimumApproximation(f, fgrad, 100, weights, -0.01).descent()

    val learnStats = nn.statistics("learn", nweights, learnExamples) * -1
    println(learnStats)
    println(learnMaxgain)
    println((lastgain, lastchange))

    val evalStats = nn.statistics("eval", nweights, evalExamples) * -1
    println(evalStats)
    println(evalMaxgain)
  }

  // println(nn)

}
