package net.stoerr.stocklearning.calculationcompiler

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
class CalculationCompiler(val calculations: Vector[CalculationItem]) {

  val groups = calculations.groupBy(_.output).map(c => new CalculationGroup(c._2)).map(simplify).toArray

  val ordered = order(groups)

  override def toString = "CalculationCompiler(ordered: \n  " + ordered.mkString("\n  ") + "\n)"

  def simplify(group: CalculationGroup): CalculationGroup = {
    val newCalcs = WeightedSumSimplifier(group.calculations.toList).toArray
    println("Simplified " + group.calculations.size + " to " + newCalcs.length)
    new CalculationGroup(newCalcs)
  }

  /** An ordering of the calculations - we process everything in one step that depends on nothing, then order the rest. */
  def order(unordered: Traversable[CalculationGroup]): List[Traversable[CalculationGroup]] = {
    if (unordered.isEmpty) return List()
    val (first, rest) = unordered.partition(c => !unordered.exists(c dependsOn _))
    return first :: order(rest)
  }

}

case class CalculationGroup(inputs: Vector[CalculationVariable], output: CalculationVariable, calculations: Vector[CalculationItem]) {
  require(!inputs.contains(output))

  def this(calcs: Traversable[CalculationItem]) = this(calcs.flatMap(_.inputs).toSet.toVector, calcs.head.output, calcs.toVector.sorted)

  override def toString = "CalculationGroup(inputs:" + inputs.mkString(",") + "; output:" + output + "; calculations: " +
    calculations.mkString(", ") + ")"

  /** Uses an output of another CalculationGroup. Not transitive; a calculation never depends on itself. */
  def dependsOn(o: CalculationGroup) = inputs.contains(o.output)
}

