package net.stoerr.stocklearning.calculationcompiler

import scala.collection.immutable

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
sealed trait CalculationItem extends Comparable[CalculationItem] {
  val output: CalculationVariable
  val inputs: immutable.IndexedSeq[CalculationVariable]

  override def compareTo(o: CalculationItem): Int = CalculationItem.calculationItemOrdering.compare(this, o)
}

object CalculationItem {
  val calculationItemOrdering: Ordering[CalculationItem] =
    Ordering.by(i => (i.getClass.toString, i.output, i.inputs.toIterable))
}

case class WeightedSum(input: CalculationVariable, weight: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " += " + input + "*" + weight

  override val inputs: immutable.IndexedSeq[CalculationVariable] = Vector(input, weight)
}

case class Cosh(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " = cosh(" + input + ")"

  override val inputs: immutable.IndexedSeq[CalculationVariable] = Vector(input)
}

case class WeightedSumCombined(inputStart: Int, inputStep: Int, weightStart: Int, weightStep: Int, count: Int, inputs: immutable.IndexedSeq[CalculationVariable], weights: immutable.IndexedSeq[CalculationVariable], output: CalculationVariable) extends CalculationItem {

  require(inputs.length == weights.length)

  override def toString = "WeightedSumCombined(istart=" + inputStart + ", istep=" + inputStep +
    ", wstart=" + weightStart + ", wstep=" + weightStep + ", count=" + count +
    ", inputs:" + inputs.mkString(", ") + "; weights:" + weights.mkString(", ") + "; output=" + output + ")"

  def this(w1: WeightedSum, w2: WeightedSum) =
    this(inputStart = w1.input.n, inputStep = w2.input.n - w1.input.n, weightStart = w1.weight.n, weightStep = w2.weight.n - w1.weight.n,
      count = 2, inputs = Vector(w1.input, w2.input), weights = Vector(w1.weight, w2.weight), output = w1.output)

  def +(w: WeightedSum) = {
    require(extendedBy(w))
    new WeightedSumCombined(inputStart = inputStart, inputStep = inputStep, weightStart = weightStart, weightStep = weightStep, count = count + 1,
      inputs = inputs :+ w.input, weights = weights :+ w.weight, output = output)
  }

  def extendedBy(w: WeightedSum): Boolean = (w.output == output) &&
    (w.input.n == inputStart + count * inputStep) && (w.weight.n == weightStart + count * weightStep)

}

object WeightedSumSimplifier {
  def isSeries(x: Int, y: Int, z: Int): Boolean = (y - x) == (z - y)

  def isSeries(x: WeightedSum, y: WeightedSum, z: WeightedSum): Boolean =
    isSeries(x.input.n, y.input.n, z.input.n) && isSeries(x.weight.n, y.weight.n, z.weight.n) && (x.output == y.output) && (y.output == z.output)

  def apply(calculations: List[CalculationItem]): List[CalculationItem] = calculations match {
    case (w1: WeightedSum) :: (w2: WeightedSum) :: (w3: WeightedSum) :: rest
      if isSeries(w1, w2, w3)
    => apply(new WeightedSumCombined(w1, w2) + w3 :: rest)
    case (wc: WeightedSumCombined) :: (w: WeightedSum) :: rest if wc.extendedBy(w) => apply(wc + w :: rest)
    case h :: tail => h :: apply(tail)
    case empty => empty
  }
}
