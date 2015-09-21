package net.stoerr.stocklearning.deepnn2

/**
 * Calculation strategy that works only for the given terms by compiling them to Java.
 */
class NNTranspilerCalculationStrategy(terms: Traversable[NNTerm]) {

  val transpiler = new NNtoJavaTranspiler(terms.toSet)

  /** Special case for only one valuation - doesn't make much sense in general. */
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] = {
    val evaluator = transpiler.makeEvaluator()
    evaluator.allInputs = Array(toArray(valuation, transpiler.inputnumber))
    evaluator.allOutputs = Array(toArray(valuation, transpiler.outputnumber))
    evaluator.allRes = Array(Array.ofDim[Double](transpiler.resultnumber.size))
    evaluator.w = toArray(valuation, transpiler.weightnumber)
    evaluator.execute(1)
    t => evaluator.allRes(0)(transpiler.resultnumber(t))
  }

  def toArray(valuation: PartialFunction[NNTerm, Double], termmap: Map[NNTerm, Int]) =
    termmap.toArray.sortBy(_._2).map(p => valuation(p._1))

}
