package net.stoerr.stocklearning.calculationcompiler

import scala.collection.mutable

/**
 * "Compiles" functions specified as Term into CalculationItems.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 19.01.2015
 */
class CalculationTermCompiler {

  private val store = new CalculationStore

  private def addToStore(item: CalculationItem): CalculationItem = {
    store += item
    item
  }

  private val compiledTerms = mutable.Map[Term, CalculationVariable]()

  /** (Cachedly) creates calculations to calculate a term */
  private def compile(term: Term): CalculationVariable = {
    def translate(): CalculationVariable = term match {
      case v: Variable => store.newVariable()
      case Constant(v) => addToStore(ConstantItem(store.newVariable(), v)).output
      case Product(f1, f2) => addToStore(WeightedSumItem(compile(f1), compile(f2), store.newVariable())).output
      case Sum(summands) => {
        val out = store.newVariable()
        summands foreach {
          case Product(f1, f2) => addToStore(WeightedSumItem(compile(f1), compile(f2), out))
          case other => addToStore(WeightedSumItem(compile(other), compile(Term.ONE), out)) // hopefully rare
        }
        out
      }
    }
    compiledTerms.getOrElseUpdate(term, translate())
  }

  def toFunction(vars: Seq[Variable], terms: Seq[Term]): Array[Double] => Array[Double] = {
    val outvars: Array[CalculationVariable] = terms.map(compile).toArray
    val invars: Seq[CalculationVariable] = vars.map(compile)
    val plan: CalculationExecutionPlan = store.executionPlan()
    return { (values: Array[Double]) =>
      val executionArea: Array[Double] = Array.fill(plan.areaSize)(0)
      (invars, values).zipped.foreach { case (v, value) => executionArea(v.n) = value}
      println(executionArea.zipWithIndex.map(z => "v" + z._2 + "=" + z._1).toList)
      plan.execute(executionArea)
      println(executionArea.zipWithIndex.map(z => "v" + z._2 + "=" + z._1).toList)
      println(plan)
      outvars.map(v => executionArea(v.n))
    }
  }

  def toFunction(vars: Seq[Variable], term: Term): Array[Double] => Double = toFunction(vars, List(term)).andThen(_(0))

}
