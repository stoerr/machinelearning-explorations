package net.stoerr.stocklearning.calculationcompiler

import net.stoerr.stocklearning.java.DoubleArrayOps

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

case class Constant(output: CalculationVariable, value: Double) extends CalculationItem {
  override def toString = output + " = " + value

  override def execute(values: Array[Double]): Unit = values(output.n) = value

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet.empty
}

case class Factor(input: CalculationVariable, output: CalculationVariable, value: Double) extends CalculationItem {
  override def toString = output + " = " + (if (1 == value) input else value + " * " + input)

  override def execute(values: Array[Double]): Unit = values(output.n) = value * values(input.n)

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input)
}

case class WeightedSum(input: CalculationVariable, weight: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " += " + input + "*" + weight

  override def execute(values: Array[Double]): Unit = values(output.n) = values(input.n) * values(weight.n)

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input, weight)
}

case class Tanh(input: CalculationVariable, output: CalculationVariable) extends CalculationItem {
  override def toString = output + " = tanh(" + input + ")"

  override def execute(values: Array[Double]): Unit = values(output.n) = math.tanh(values(input.n))

  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet(input)
}

case class WeightedSumCombined(firstStart: Int, firstStep: Int, secondStart: Int, secondStep: Int, count: Int, first: immutable.IndexedSeq[CalculationVariable], second: immutable.IndexedSeq[CalculationVariable], output: CalculationVariable) extends CalculationItem {

  require(first.length == second.length)
  override val inputs: immutable.SortedSet[CalculationVariable] = TreeSet[CalculationVariable]() ++ first ++ second

  override def execute(values: Array[Double]): Unit =
    values(output.n) = DoubleArrayOps.dotProduct(count, values, firstStart, firstStep, values, secondStart, secondStep)

  override def toString = "WeightedSumCombined(istart=" + firstStart + ", istep=" + firstStep +
    ", wstart=" + secondStart + ", wstep=" + secondStep + ", count=" + count +
    ", first:" + first.mkString(", ") + "; second:" + second.mkString(", ") + "; output=" + output + ")"

  def this(w1: WeightedSum, w2: WeightedSum) =
    this(firstStart = w1.input.n, firstStep = w2.input.n - w1.input.n, secondStart = w1.weight.n, secondStep = w2.weight.n - w1.weight.n,
      count = 2, first = Vector(w1.input, w2.input), second = Vector(w1.weight, w2.weight), output = w1.output)

  def +(w: WeightedSum) = {
    require(extendedBy(w))
    new WeightedSumCombined(firstStart = firstStart, firstStep = firstStep, secondStart = secondStart, secondStep = secondStep, count = count + 1,
      first = first :+ w.input, second = second :+ w.weight, output = output)
  }

  def extendedBy(w: WeightedSum): Boolean = (w.output == output) &&
    (w.input.n == firstStart + count * firstStep) && (w.weight.n == secondStart + count * secondStep)

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
