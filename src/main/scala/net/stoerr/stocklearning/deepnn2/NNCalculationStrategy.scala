package net.stoerr.stocklearning.deepnn2

import collection.immutable.TreeMap
import collection.parallel.ParSeq

/**
 * Reference calculation strategy - not particularily efficient
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.09.2015
 */
object NNSimpleCalculationStrategy extends SNNDoubleEvaluator {
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] =
    new NNSimpleEvaluator(valuation).eval

  def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double])
  : Function[SNNTerm, Double]
  = new SNNSimpleEvaluator(valuations, restValuation).eval
}

/** Calculation strategy that caches common subterms and is somewhat parallelized. Still not the most efficient
  * way to do it, since the cache is not very efficient */
object NNCachedCalculationStrategy extends SNNDoubleEvaluator {
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] =
    new NNCachedEvaluator(valuation).eval

  def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double]):
  Function[SNNTerm, Double]
  = new SNNCachedEvaluator(valuations, restValuation).eval
}


trait SNNDoubleEvaluator {

  /** returns function of weights to value and vector of derivative of value by weight */
  def asDerivedFunction(term: SNNTerm, inputs: Traversable[IndexedSeq[Double]], outputs:
  Traversable[IndexedSeq[Double]]):
  Function[IndexedSeq[Double], (Double, IndexedSeq[Double])] = {
    val derivative = term.wDerivative
    val weightVars = term.weights
    val valuations = outputs.map(values => toValuation(term.outputs, values)) ++
      inputs.map(values => toValuation(term.inputs, values))
    weights: IndexedSeq[Double] => {
      val restValuation = toValuation(weightVars, weights)
      val evalFunction: Function[SNNTerm, Double] = eval(valuations, restValuation)
      (evalFunction(term), weightVars.map(derivative).map(evalFunction))
    }
  }

  private def toValuation[T <: NNTerm](variables: Array[T], values: IndexedSeq[Double]):
  PartialFunction[NNTerm, Double] = variables.zip(values).toMap

  def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double]):
  Function[SNNTerm, Double]

}

private class NNSimpleEvaluator(valuation: PartialFunction[NNTerm, Double]) {

  def eval(term: NNTerm): Double = term match {
    case C(v) => v
    case v if valuation.isDefinedAt(v) => valuation(v)
    case Sum(summands) => summands.map((term: NNTerm) => eval(term)).sum
    case Prod(p1, p2) => eval(p1) * eval(p2)
    case Tanh(t) => math.tanh(eval(t))
    case SumProd(prods) => prods.map(p => eval(p._1) * eval(p._2)).sum
  }

}

private class SNNSimpleEvaluator(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation:
PartialFunction[NNTerm, Double]) {

  def eval(valuation: PartialFunction[NNTerm, Double])(term: NNTerm): Double =
    new NNSimpleEvaluator(valuation).eval(term)

  def eval(term: SNNTerm): Double
  = term match {
    case SC(v) => v
    case SSum(summands) => summands.map((term: SNNTerm) => eval(term)).sum
    case SProd(p1, p2) => eval(p1) * eval(p2)
    case SUMMED(t) => valuations.map(valuation => eval(valuation orElse restValuation)(t)).sum
  }

}

private class NNCachedEvaluator(valuation: PartialFunction[NNTerm, Double]) extends NNSimpleEvaluator(valuation) {
  @volatile var cacheNN: TreeMap[NNTerm, Double] = TreeMap()

  override def eval(term: NNTerm): Double = {
    val cached = cacheNN.get(term)
    if (cached.isDefined) cached.get
    else {
      val value = super.eval(term)
      cacheNN = cacheNN.updated(term, value)
      value
    }
  }

}

private class SNNCachedEvaluator(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation:
PartialFunction[NNTerm, Double]) extends SNNSimpleEvaluator(valuations, restValuation) {

  @volatile var cacheSNN: TreeMap[SNNTerm, Double] = TreeMap()
  val summedEvaluators: ParSeq[NNCachedEvaluator] =
    valuations.toArray.map(valuation => new NNCachedEvaluator(valuation orElse restValuation)).par

  override def eval(term: SNNTerm): Double = {
    val cached = cacheSNN.get(term)
    if (cached.isDefined) cached.get
    else term match {
      case SUMMED(t) => summedEvaluators.map(_.eval(t)).sum
      case _ =>
        val value = super.eval(term)
        cacheSNN = cacheSNN.updated(term, value)
        value
    }
  }
}
