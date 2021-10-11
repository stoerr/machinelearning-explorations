package net.stoerr.learning.learnalgorithmexplorations.deepnn2

import net.stoerr.learning.gpunn.java.AbstractNNJavaEvaluator

import scala.collection.convert.ImplicitConversions.`iterator asScala`
import scala.collection.mutable
import scala.concurrent.JavaConversions

/**
 * Calculation strategy that works only for the given terms by compiling them to Java.
 */
class NNTranspilerCalculationStrategy(terms: Traversable[NNTerm]) extends SNNDoubleEvaluator {

  val transpiler = new NNtoJavaTranspiler(terms.toSet)

  /** Special case for only one valuation - doesn't make much sense in general. */
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] = {
    val evaluator = transpiler.makeEvaluator()
    evaluator.inSubSize = transpiler.inputnumber.size
    evaluator.outSubSize = transpiler.outputnumber.size
    evaluator.resSubSize = transpiler.resultnumber.size
    evaluator.in = toArray(valuation, transpiler.inputnumber)
    evaluator.out = toArray(valuation, transpiler.outputnumber)
    evaluator.w = toArray(valuation, transpiler.weightnumber)
    evaluator.res = Array.ofDim[Float](transpiler.resultnumber.size)
    evaluator.sanityCheck(1)
    evaluator.execute(1)
    evaluator.dispose()
    t => evaluator.res(transpiler.resultnumber(t))
  }

  private def toArray(valuation: PartialFunction[NNTerm, Double], termmap: Map[NNTerm, Int]): Array[Float] =
    termmap.toArray.sortBy(_._2).map(p => valuation(p._1).toFloat)

  override def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm,
    Double]): (SNNTerm) => Double = {
    val summedTerms: Map[NNTerm, Double] = {
      val evaluator = transpiler.makeEvaluator()
      evaluator.inSubSize = transpiler.inputnumber.size
      evaluator.outSubSize = transpiler.outputnumber.size
      evaluator.resSubSize = transpiler.resultnumber.size
      evaluator.in = valuations.toArray.flatMap(toArray(_, transpiler.inputnumber))
      evaluator.out = valuations.toArray.flatMap(toArray(_, transpiler.outputnumber))
      evaluator.w = toArray(restValuation, transpiler.weightnumber)
      evaluator.res = Array.ofDim[Float](valuations.size * transpiler.resultnumber.size)
      evaluator.sanityCheck(valuations.size)
      evaluator.execute(valuations.size)
      println("Execution mode = " + evaluator.getExecutionMode)
      printstats(evaluator)
      evaluator.dispose()
      val results = evaluator.res.grouped(transpiler.resultnumber.size).toArray.transpose.map(_.sum)
      transpiler.resultnumber.mapValues(r => results(r).toDouble).toMap
    }

    val snnEvaluator = new SNNSimpleEvaluator(valuations, restValuation) {

      val cacheSNN: mutable.Map[SNNTerm, Double] = mutable.HashMap()

      override def eval(term: SNNTerm): Double =
        cacheSNN.getOrElseUpdate(term,
          term match {
            case SUMMED(t) => summedTerms(t)
            case _ => super.eval(term)
          }
        )

      override def eval(valuation: PartialFunction[NNTerm, Double])(term: NNTerm): Double = sys.error("should be " +
        "unused")
    }

    snnTerm => snnEvaluator.eval(snnTerm)
  }

  def printstats(evaluator: AbstractNNJavaEvaluator) = {
    val profileInfo = evaluator.getProfileInfo
    if (null != profileInfo) profileInfo.iterator().toArray.foreach(println)
  }

}
