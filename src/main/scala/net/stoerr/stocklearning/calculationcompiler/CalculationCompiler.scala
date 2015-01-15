package net.stoerr.stocklearning.calculationcompiler

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
class CalculationCompiler(val calculations: Array[CalculationItem]) {

  val groups = calculations.groupBy(_.output).map { case (output, calcs) =>
    CalculationGroup(calcs.flatMap(_.inputs).toSet.toArray, output, calcs.sorted)
  }.map(simplify).toArray

  override def toString = "CalculationCompiler(\n  " + groups.mkString("\n  ") + "\n)"

  def simplify(group: CalculationGroup): CalculationGroup = {
    group
  }

}

case class CalculationGroup(inputs: Array[CalculationVariable], output: CalculationVariable, calculations: Array[CalculationItem]) {
  override def toString = "CalculationGroup(inputs:" + inputs.mkString(",") + "; output:" + output + "; calculations: " +
    calculations.mkString(", ") + ")"
}

