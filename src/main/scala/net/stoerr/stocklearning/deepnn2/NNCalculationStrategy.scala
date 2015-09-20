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
  : SNNTerm => Double
  = new SNNSimpleEvaluator(valuations, restValuation).eval
}

/** Calculation strategy that caches common subterms and is somewhat parallelized. Still not the most efficient
  * way to do it, since the cache is not very efficient */
object NNCachedCalculationStrategy extends SNNDoubleEvaluator {
  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double] =
    new NNCachedEvaluator(valuation).eval

  def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double]):
  SNNTerm => Double
  = new SNNCachedEvaluator(valuations, restValuation).eval
}

trait SNNDoubleEvaluator {

  /** returns function of weights to value and vector of derivative of value by weight */
  def asDerivedFunction[LDOUBLE <: Seq[Double]](term: SNNTerm, inputOutput: Traversable[(LDOUBLE, LDOUBLE)]):
  Array[Double] => (Double, Array[Double]) = {
    val derivative = term.wDerivative
    val weightVars = term.weights
    val valuations = inputOutput.map(values => toValuation(term.inputs, values._1) orElse toValuation(term.outputs,
      values._2))
    weights: Array[Double] => {
      val restValuation = toValuation(weightVars, weights)
      val evalFunction: SNNTerm => Double = eval(valuations, restValuation)
      (evalFunction(term), weightVars.map(derivative).map(evalFunction))
    }
  }

  private def toValuation[T <: NNTerm](variables: Array[T], values: Seq[Double]):
  PartialFunction[NNTerm, Double] = {
    require(variables.length == values.length, "vars: " + variables.toList + " , vals: " + values)
    variables.zip(values).toMap
  }

  def asResultFunction(terms: Traversable[NNTerm], weights: Array[Double]): Array[Double] => Array[Double] = {
    val inputVars = terms.flatMap(_.inputs).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted
    val weightVars = terms.flatMap(_.weights).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted
    val weightValuation = toValuation(weightVars, weights)
    require(terms.flatMap(_.outputs).isEmpty)
    inputs => {
      require(inputVars.length == inputs.length, "inputVars:" + inputVars.toList + " , inputs: " + inputs.toList)
      val func = eval(toValuation(inputVars, inputs) orElse weightValuation)
      terms.map(func(_)).toArray
    }
  }

  def asResultFunction(term: NNTerm, weights: Array[Double]): Array[Double] => Double = asResultFunction(List(term),
    weights)
    .andThen(_(0))

  def eval(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation: PartialFunction[NNTerm, Double]):
  SNNTerm => Double

  def eval(valuation: PartialFunction[NNTerm, Double]): Function[NNTerm, Double]

}

private class NNSimpleEvaluator(valuation: PartialFunction[NNTerm, Double]) {

  def eval(term: NNTerm): Double = term match {
    case C(v) => v
    case i@I(_) => valuation(i)
    case o@O(_) => valuation(o)
    case w@W(_) => valuation(w)
    // case v if valuation.isDefinedAt(v) => valuation(v)
    case Sum(summands) => summands.map((term: NNTerm) => eval(term)).sum
    case Prod(p1, p2) => eval(p1) * eval(p2)
    case Tanh(t) => math.tanh(eval(t))
    case RLin(t) => val te = eval(t); if (te > 0) te else 0 // XXX if (te > 0) te else 0
    case Step(t) => val te = eval(t); if (te > 0) 1 else 0 // XXX if (te > 0) 1 else 0
    case Sqr(t) => val te = eval(t); te * te
    case SoftSign(t) => val te = eval(t); te / (1 + math.abs(te))
    case SoftSignD(t) => val tex = 1 + math.abs(eval(t)); 1 / (tex * tex)
    case SumProd(prods) => prods.map(p => eval(p._1) * eval(p._2)).sum
  }

}

private class SNNSimpleEvaluator(valuations: Traversable[PartialFunction[NNTerm, Double]], restValuation:
PartialFunction[NNTerm, Double]) {

  def eval(valuation: PartialFunction[NNTerm, Double])(term: NNTerm): Double =
    new NNSimpleEvaluator(valuation).eval(term)

  def eval(term: SNNTerm): Double = term match {
    case SC(v) => v
    case SSum(summands) => summands.map((term: SNNTerm) => eval(term)).sum
    case SProd(p1, p2) => eval(p1) * eval(p2)
    case SUMMED(t) => valuations.par.map(valuation => eval(valuation orElse restValuation)(t)).sum
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
  val summedEvaluators: ParSeq[NNCachedEvaluator] = valuations.toArray.map(valuation => new NNCachedEvaluator
  (valuation orElse restValuation)).par

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
