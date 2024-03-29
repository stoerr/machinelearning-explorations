package net.stoerr.learning.gpunn.calculationcompiler

import scala.collection.parallel.CollectionConverters._


/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
class CalculationExecutionPlan(val calculations: Vector[CalculationItem]) {

  val groups: Array[CalculationGroup] = calculations.groupBy(_.output).map(c => new CalculationGroup(c._2)).map(simplify).toArray

  /** A list of "levels": sets of independent CalculationGroups that can be executed in parallel. */
  val ordered: List[Traversable[CalculationGroup]] = order(groups)

  val areaSize = math.max(calculations.map(_.output.n).max, calculations.flatMap(_.inputs).map(_.n).max) + 1

  override def toString = "CalculationExecutionPlan(ordered (" + calculations.size + " -> " + groups.size + " -> " + ordered.size + ") : \n  " +
    ordered.map(_.map(_.calculations.mkString(", ")).mkString(";  ")).mkString("\n  ") + "\n)"

  private def simplify(group: CalculationGroup): CalculationGroup = {
    val newCalcs = WeightedSumSimplifier(group.calculations.toList).toArray
    println("Simplified " + group.calculations.size + " to " + newCalcs.length)
    if (group.calculations.size == newCalcs.length) println(newCalcs.mkString(",  "))
    new CalculationGroup(newCalcs)
  }

  /** An ordering of the calculations - we process everything in one step that depends on nothing, then order the rest. */
  private def order(unordered: Traversable[CalculationGroup]): List[Traversable[CalculationGroup]] = {
    if (unordered.isEmpty) return List()
    val (first, rest) = unordered.partition(c => !unordered.exists(c dependsOn _))
    return first :: order(rest)
  }

  def execute(executionArea: Array[Double]): Unit = {
    ordered.seq.foreach { level: Traversable[CalculationGroup] =>
      level.par.foreach { group: CalculationGroup =>
        group.execute(executionArea)
      }
    }
  }

}

/** A number of calculations with a common output that can be executed in any order, but serially */
case class CalculationGroup(inputs: Vector[CalculationVariable], output: CalculationVariable, calculations: Vector[CalculationItem]) {
  require(!inputs.contains(output))

  def this(calcs: Traversable[CalculationItem]) = this(calcs.flatMap(_.inputs).toSet.toVector, calcs.head.output, calcs.toVector.sorted)

  override def toString = "CalculationGroup(inputs:" + inputs.mkString(",") + "; output:" + output + "; calculations: " +
    calculations.mkString(", ") + ")"

  /** Uses an output of another CalculationGroup. Not transitive; a calculation never depends on itself. */
  def dependsOn(o: CalculationGroup) = inputs.contains(o.output)

  def execute(values: Array[Double]): Unit = calculations.seq.foreach(_.execute(values))

}
