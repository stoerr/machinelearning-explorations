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

  def compile() = new CalculationCompiler(calculations.toArray)
}

case class CalculationVariable(n: Int) {
  override def toString() = "v" + n
}

sealed trait CalculationItem {
  val output: CalculationVariable
  val inputs: Traversable[CalculationVariable]
}

case class WeightedSum(input: CalculationVariable, weight: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString() = output + " += " + input + "*" + weight

  override val inputs: Traversable[CalculationVariable] = Array(input, weight)
}

case class Cosh(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString() = output + " = cosh(" + input + ")"

  override val inputs: Traversable[CalculationVariable] = Array(input)
}

case class CalculationGroup(inputs: Array[CalculationVariable], output: CalculationVariable, calculations: Array[CalculationItem]) {
  override def toString() = "CalculationGroup(inputs:" + inputs.mkString(",") + "; output:" + output + "; calculations: " +
    calculations.mkString(", ") + ")"
}

class CalculationCompiler(val calculations: Array[CalculationItem]) {

  val groups = calculations.groupBy(_.output).map { case (output, calcs) =>
    CalculationGroup(calcs.flatMap(_.inputs), output, calcs)
  }.toArray

  override def toString() = "CalculationCompiler(\n  " + groups.mkString("\n  ") + "\n)"
}
