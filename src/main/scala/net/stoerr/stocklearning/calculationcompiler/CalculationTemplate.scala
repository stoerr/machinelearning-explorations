package net.stoerr.stocklearning.calculationcompiler

import scala.collection.mutable.ArrayBuffer

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.01.2015
 */
class CalculationTemplate {
  val calculations = new ArrayBuffer[CalculationItem]
  private var runningNumberOfVariable = -1

  def +=(calculation: CalculationItem): this.type = {
    calculations += calculation
    this
  }

  def ++=(newCalculations: TraversableOnce[CalculationItem]): this.type = {
    calculations ++= newCalculations
    this
  }

  def newVariable() = {
    runningNumberOfVariable += 1
    new CalculationVariable(runningNumberOfVariable)
  }

  def newInput() = newVariable()

  def newOutput() = newVariable()

  override def toString = "CalculationTemplate[" + calculations + "]"

  def compile() = new CalculationCompiler(calculations.toVector)

  /** gives the possibility to write a calculation with operators */
  case class CalculationTerm(calculation: CalculationItem) {
    CalculationTemplate.this += calculation

    def this(variable: CalculationVariable) = this(Factor(variable, newVariable(), 1))

    def *(o: CalculationTerm) = WeightedSum(calculation.output, o.calculation.output, newVariable())
  }

  def toTerm(v: CalculationVariable) = new CalculationTerm(v)

}

case class CalculationVariable(n: Int) extends Comparable[CalculationVariable] {
  override def toString = "v" + n

  override def compareTo(o: CalculationVariable): Int = n.compareTo(o.n)

}

object CalculationVariable {
  implicit def toCalculationTerm(v: CalculationVariable)(implicit template: CalculationTemplate) = template.toTerm(v)
}
