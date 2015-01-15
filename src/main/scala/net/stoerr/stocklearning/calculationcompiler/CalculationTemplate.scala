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
}

case class CalculationVariable(n: Int) extends Comparable[CalculationVariable] {
  override def toString = "v" + n

  override def compareTo(o: CalculationVariable): Int = n.compareTo(o.n)
}