package net.stoerr.stocklearning.calculationcompiler

import scala.collection.mutable.ArrayBuffer

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 14.01.2015
 */
class CalculationTemplate {
  val calculations = new ArrayBuffer[CalculationItem]
  private var runningNumberOfVariable = -1;

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

  override def toString() = "CalculationTemplate[" + calculations + "]"
}

case class CalculationVariable(n: Int) {
  override def toString() = "v" + n
}

sealed trait CalculationItem

case class Sum(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString() = output + " += " + input
}

case class Cosh(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString() = output + " = cosh(" + input + ")"
}
