package net.stoerr.learning.gpunn.calculationcompiler

import net.stoerr.learning.learnalgorithmexplorations.java.DoubleArrayOps

import scala.collection.immutable
import scala.collection.immutable.TreeSet

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 15.01.2015
 */
sealed trait CalculationItem extends Comparable[CalculationItem] {
  val output: CalculationVariable
  val inputs: immutable.SortedSet[CalculationVariable]

  def execute(values: Array[Double]): Unit

  override def compareTo(o: CalculationItem): Int = CalculationItem.calculationItemOrdering.compare(this, o)
}

object CalculationItem {
  val calculationItemOrdering: Ordering[CalculationItem] =
    Ordering.by(i => (i.getClass.toString, i.output, i.inputs.toIterable))
}

case class ConstantItem(output: CalculationVariable, value: Double) extends CalculationItem {
  override def toString = output + " = " + value

  override def execute(values: Array[Double]): Unit = values(output.n) = value

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet.empty
}

case class FactorItem(input: CalculationVariable, output: CalculationVariable, value: Double) extends CalculationItem {
  override def toString = output + " = " + (if (1 == value) input else value + " * " + input)

  override def execute(values: Array[Double]): Unit = values(output.n) = value * values(input.n)

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input)
}

case class WeightedSumItem(input: CalculationVariable, weight: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " += " + input + "*" + weight

  override def execute(values: Array[Double]): Unit = values(output.n) += values(input.n) * values(weight.n)

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input, weight)
}

case class UnaryFunctionItem(name: String, function: Double => Double, input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " = " + name + "(" + input + ")"

  override def execute(values: Array[Double]): Unit = values(output.n) = function(values(input.n))

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input)
}

case class WeightedSumCombinedItem(firstStart: Int, firstStep: Int, secondStart: Int, secondStep: Int, count: Int, first: immutable.IndexedSeq[CalculationVariable], second: immutable.IndexedSeq[CalculationVariable], output: CalculationVariable) extends CalculationItem {

  require(first.length == second.length)
  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet[CalculationVariable]() ++ first ++ second

  override def execute(values: Array[Double]): Unit =
    values(output.n) += DoubleArrayOps.dotProduct(count, values, firstStart, firstStep, values, secondStart, secondStep)

  override def toString = "WeightedSumCombined(istart=" + firstStart + ", istep=" + firstStep +
    ", wstart=" + secondStart + ", wstep=" + secondStep + ", count=" + count +
    ", first:" + first.mkString(", ") + "; second:" + second.mkString(", ") + "; output=" + output + ")"

  def this(w1: WeightedSumItem, w2: WeightedSumItem) =
    this(firstStart = w1.input.n, firstStep = w2.input.n - w1.input.n, secondStart = w1.weight.n, secondStep = w2.weight.n - w1.weight.n,
      count = 2, first = Vector(w1.input, w2.input), second = Vector(w1.weight, w2.weight), output = w1.output)

  def +(w: WeightedSumItem) = {
    require(extendedBy(w))
    new WeightedSumCombinedItem(firstStart = firstStart, firstStep = firstStep, secondStart = secondStart, secondStep = secondStep, count = count + 1,
      first = first :+ w.input, second = second :+ w.weight, output = output)
  }

  def extendedBy(w: WeightedSumItem): Boolean = (w.output == output) &&
    (w.input.n == firstStart + count * firstStep) && (w.weight.n == secondStart + count * secondStep)

}

object WeightedSumSimplifier {
  def isSeries(x: Int, y: Int, z: Int): Boolean = (y - x) == (z - y)

  def isSeries(x: WeightedSumItem, y: WeightedSumItem, z: WeightedSumItem): Boolean =
    isSeries(x.input.n, y.input.n, z.input.n) && isSeries(x.weight.n, y.weight.n, z.weight.n) && (x.output == y.output) && (y.output == z.output)

  def apply(calculations: List[CalculationItem]): List[CalculationItem] = calculations match {
    case (w1: WeightedSumItem) :: (w2: WeightedSumItem) :: (w3: WeightedSumItem) :: rest
      if isSeries(w1, w2, w3)
    => apply(new WeightedSumCombinedItem(w1, w2) + w3 :: rest)
    case (wc: WeightedSumCombinedItem) :: (w: WeightedSumItem) :: rest if wc.extendedBy(w) => apply(wc + w :: rest)
    case h :: tail => h :: apply(tail)
    case empty => empty
  }
}
