package net.stoerr.stocklearning.calculationcompiler

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
class CalculationCompiler(val calculations: Vector[CalculationItem]) {

  val groups = calculations.groupBy(_.output).map(c => new CalculationGroup(c._2)).map(simplify).toArray

  override def toString = "CalculationCompiler(\n  " + groups.mkString("\n  ") + "\n)"

  def simplify(group: CalculationGroup): CalculationGroup = {
    val newCalcs = WeightedSumSimplifier(group.calculations.toList).toArray
    new CalculationGroup(newCalcs)
  }

}

case class CalculationGroup(inputs: Vector[CalculationVariable], output: CalculationVariable, calculations: Vector[CalculationItem]) {

  def this(calcs: Traversable[CalculationItem]) = this(calcs.flatMap(_.inputs).toSet.toVector, calcs.head.output, calcs.toVector.sorted)

  override def toString = "CalculationGroup(inputs:" + inputs.mkString(",") + "; output:" + output + "; calculations: " +
    calculations.mkString(", ") + ")"
}

