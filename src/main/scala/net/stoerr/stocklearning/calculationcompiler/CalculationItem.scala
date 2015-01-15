package net.stoerr.stocklearning.calculationcompiler

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
sealed trait CalculationItem extends Comparable[CalculationItem] {
  val output: CalculationVariable
  val inputs: IndexedSeq[CalculationVariable]

  override def compareTo(o: CalculationItem): Int = CalculationItem.calculationItemOrdering.compare(this, o)
}

object CalculationItem {
  val calculationItemOrdering: Ordering[CalculationItem] =
    Ordering.by(i => (i.getClass.toString, i.output, i.inputs.toIterable))
}

case class WeightedSum(input: CalculationVariable, weight: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " += " + input + "*" + weight

  override val inputs: IndexedSeq[CalculationVariable] = Array(input, weight)
}

case class Cosh(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " = cosh(" + input + ")"

  override val inputs: IndexedSeq[CalculationVariable] = Array(input)
}

case class WeightedSumCombined(inputStart: Int, inputStep: Int, weightStart: Int, weightStep: Int, count: Int, inputs: IndexedSeq[CalculationVariable], weights: IndexedSeq[CalculationVariable], output: CalculationVariable) extends CalculationItem {

  require(inputs.length == weights.length)

  override def toString = "WeightedSumCombined(istart=" + inputStart + ", istep=" + inputStep +
    ", wstart=" + weightStart + ", wstep=" + weightStep + ", count=" + count +
    "inputs:" + inputs.mkString(", ") + "; weights:" + weights.mkString(", ") + "; output=" + output + ")"
}


