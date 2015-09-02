package net.stoerr.stocklearning.deepnn2

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.09.2015
 */
class NNCalculationStrategy {

  type Valuation = PartialFunction[NNTerm, Double]

  def eval(term: NNTerm, valuation: Valuation): Double = term match {
    case C(v) => v
    case v if valuation.isDefinedAt(v) => valuation(v)
    case Sum(summands) => summands.map(eval(_, valuation)).sum
    case Prod(p1, p2) => eval(p1, valuation) * eval(p2, valuation)
    case Tanh(t) => math.tanh(eval(t, valuation))
  }

  def eval(term: SNNTerm, valuations: Traversable[Valuation], restValuation: Valuation): Double
  = term match {
    case SC(v) => v
    case SSum(summands) => summands.map(eval(_, valuations, restValuation)).sum
    case SProd(p1, p2) => eval(p1, valuations, restValuation) * eval(p2, valuations, restValuation)
    case SUMMED(t) => valuations.map(valuation => eval(t, valuation orElse restValuation)).sum
  }

}

object NNCalculationStrategy extends NNCalculationStrategy

class NNCachedCalculationStrategy extends NNCalculationStrategy {
  // def eval(term: NNTerm, valuation: Valuation): Double =
}
