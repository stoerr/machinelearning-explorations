package net.stoerr.stocklearning.deepnn2

import scala.collection.mutable

/**
 * Calculation strategy that works only for the given terms by compiling them to Java.
 */
class NNTranspilerCalculationStrategy(terms: Traversable[NNTerm]) extends SNNDoubleEvaluator {

  val transpiler = new NNtoJavaTranspiler(terms.toSet)

  /** Special case for only one valuation - doesn't make much sense in general. */
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] = {
    val evaluator = transpiler.makeEvaluator()
    evaluator.allInputs = Array(toArray(valuation, transpiler.inputnumber))
    evaluator.allOutputs = Array(toArray(valuation, transpiler.outputnumber))
    evaluator.w = toArray(valuation, transpiler.weightnumber)
    evaluator.allRes = Array(Array.ofDim[Double](transpiler.resultnumber.size))
    evaluator.allMem = Array(Array.ofDim[Double](transpiler.maxmemlength))
    evaluator.execute(1)
    evaluator.dispose()
    t => evaluator.allRes(0)(transpiler.resultnumber(t))
  }

  private def toArray(valuation: PartialFunction[NNTerm, Double], termmap: Map[NNTerm, Int]) =
    termmap.toArray.sortBy(_._2).map(p => valuation(p._1))

  override def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double]): (SNNTerm) => Double = {
    val summedTerms: Map[NNTerm, Double] = {
      val evaluator = transpiler.makeEvaluator()
      evaluator.allInputs = valuations.toArray.map(toArray(_, transpiler.inputnumber))
      evaluator.allOutputs = valuations.toArray.map(toArray(_, transpiler.outputnumber))
      evaluator.w = toArray(restValuation, transpiler.weightnumber)
      evaluator.allRes = Array.ofDim[Double](valuations.size, transpiler.resultnumber.size)
      evaluator.allMem = Array.ofDim[Double](valuations.size, transpiler.maxmemlength)
      evaluator.execute(valuations.size)
      println("Execution mode = "+evaluator.getExecutionMode)
      evaluator.dispose()
      val results = evaluator.allRes.transpose.map(_.sum)
      transpiler.resultnumber.mapValues(results)
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

      override def eval(valuation: PartialFunction[NNTerm, Double])(term: NNTerm): Double = sys.error("should be unused")
    }

    snnTerm => snnEvaluator.eval(snnTerm)
  }

}
