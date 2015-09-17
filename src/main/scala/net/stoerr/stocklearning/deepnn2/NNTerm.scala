package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.deepnn2.NNTerm._
import net.stoerr.stocklearning.deepnn2.SNNTerm._

import scala.language.implicitConversions
import scala.util.hashing.MurmurHash3._

/**
 * Definition of terms that can be used to implement neural network learning.
 * TODO: cached implementation of evaluation (parallel for SUMMED -> careful with wDerivative); do neural network.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */

sealed trait NNTermBase {
  /** Base type of the subterms of this term - either NNTerm or SNNTerm */
  type TermComponent >: this.type <: NNTermBase

  /** Immediate subterms */
  def subterms: Seq[TermComponent]

  def componentStream: Stream[NNTermBase] = this #:: subterms.toStream.flatMap(_.componentStream)

  def inputs = componentStream.filter(_.isInstanceOf[I]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[I])

  def outputs = componentStream.filter(_.isInstanceOf[O]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[O])

  def weights = componentStream.filter(_.isInstanceOf[W]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[W])

  def toChars: Iterator[Char]

  override def toString = toChars.mkString

  protected val theHashCode: Int

  override final def hashCode: Int = theHashCode

  def compare(o: TermComponent): Int = {
    val (it1, it2) = (this.toChars, o.toChars)
    while (it1.hasNext && it2.hasNext) {
      val (c1, c2) = (it1.next(), it2.next())
      if (0 != c1.compareTo(c2)) return c1.compareTo(c2)
    }
    if (it1.hasNext) 1 else if (it2.hasNext) -1; else 0
  }
}

sealed trait NNTerm extends NNTermBase with Ordered[NNTerm] {
  override type TermComponent = NNTerm

  def +(o: NNTerm): NNTerm = {
    def summandlist(t: NNTerm): Seq[NNTerm] = t match {
      case Sum(summands) => summands
      case _ => Vector(t)
    }
    Sum((summandlist(this) ++ summandlist(o)).sorted)
  }

  def -(o: NNTerm): NNTerm = this + o * -1.0

  def *(o: NNTerm): NNTerm =
    if (this == ZERO) ZERO
    else if (this == ONE) o
    else if (o == ZERO) this
    else if (o == ONE) this
    else if (this == o) Sqr(this)
    else if (this > o) Prod(o, this) else Prod(this, o)

  protected def sumDerivatives(derivatives: Traversable[(W, NNTerm)]): Map[W, NNTerm] =
    derivatives.groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.reduce(_ + _))

  def wDerivative: Map[W, NNTerm]

  def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm

}

object NNTerm {
  implicit def c(value: Double): C = C(value)

  implicit def c(value: Int): C = C(value)


  def sumProd(s: IndexedSeq[(NNTerm, NNTerm)]) = {
    def sort(p: (NNTerm, NNTerm)): (NNTerm, NNTerm) = if (p._1 > p._2) (p._2, p._1) else (p._1, p._2)
    SumProd(s.map(sort).sortBy(p => p._1 * p._2).toVector)
  }

  val ZERO = C(0)
  val ONE = C(1)
}

protected abstract class ConstantNNTerm(name: String) extends NNTerm {

  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = if (s.isDefinedAt(this)) s(this) else this

  override def wDerivative: Map[W, NNTerm] = Map()

  override def subterms: Seq[TermComponent] = Stream()

  override def toChars = name.iterator

  protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), stringHash(name)), 0)

}

case class W(name: String) extends ConstantNNTerm("W" + name) {
  override def wDerivative: Map[W, NNTerm] = Map(this -> ONE)
}

case class I(name: String) extends ConstantNNTerm("I" + name)

case class O(name: String) extends ConstantNNTerm("O" + name)

case class C(value: Double) extends ConstantNNTerm(value.toString)

case class Sum(summands: Seq[NNTerm]) extends NNTerm {

  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = Sum(summands.map(_.subst(s)))

  override def wDerivative: Map[W, NNTerm] = sumDerivatives(summands.flatMap(_.wDerivative))

  override def subterms: Seq[TermComponent] = summands

  override def toChars = "(".toIterator ++ summands.toIterator.map(_.toChars).reduce(_ ++ " + ".toIterator ++ _) ++
    ")".toIterator

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), seqHash(summands)), 0)

}

case class SumProd(summands: IndexedSeq[(NNTerm, NNTerm)]) extends NNTerm {

  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm =
    SumProd(summands.map(p => (p._1.subst(s), p._2.subst(s))))

  override def wDerivative: Map[W, NNTerm] =
    sumDerivatives(summands.flatMap(p => p._1.wDerivative.mapValues(_ * p._2)) ++
      summands.flatMap(p => p._2.wDerivative.mapValues(_ * p._1)))

  override def subterms: Seq[TermComponent] = summands.flatMap(p => Seq(p._1, p._2))

  override def toChars = "(".toIterator ++ summands.toIterator.map(s => s._1.toChars ++ " * ".toIterator ++ s._2
    .toChars).reduce(_ ++ " + ".toIterator ++ _) ++ ")".toIterator

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), seqHash(summands)), 0)
}

case class Prod(p1: NNTerm, p2: NNTerm) extends NNTerm {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = Prod(p1.subst(s), p2.subst(s))

  override def wDerivative: Map[W, NNTerm] =
    sumDerivatives(p1.wDerivative.mapValues(_ * p2).toSeq ++ p2.wDerivative.mapValues(_ * p1).toSeq)

  override def subterms: Seq[TermComponent] = Seq(p1, p2)

  override def toChars = p1.toChars ++ " * ".toIterator ++ p2.toChars

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), mix(p1.hashCode, p2.hashCode)), 0)
}

protected abstract class FunctionNNTerm(argument: NNTerm) extends NNTerm {
  override def subterms: Seq[TermComponent] = Stream(argument)

  override def toChars = (getClass.getSimpleName + "(").toIterator ++ argument.toChars ++ ")".toIterator

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), argument.hashCode), 0)
}

case class Tanh(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = Tanh(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = t.wDerivative.mapValues(_ * (1 - this * this))
}

/** Rectilinear */
case class RLin(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = RLin(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = t.wDerivative.mapValues(_ * Step(t))
}

/** Derivation of rectilinear : 1 if > 0, 0 else */
case class Step(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = Step(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = sys.error(s"Derivative of ${getClass.getSimpleName} undefined")
}

case class Sqr(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = Sqr(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = t.wDerivative.mapValues(_ * t * 2)
}

case class SoftSign(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = SoftSign(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = t.wDerivative.mapValues(_ * SoftSignD(t))
}

/** Derivation of SoftSign */
case class SoftSignD(t: NNTerm) extends FunctionNNTerm(t) {
  override def subst(s: PartialFunction[NNTerm, NNTerm]): NNTerm = SoftSignD(t.subst(s))

  override def wDerivative: Map[W, NNTerm] = sys.error(s"Derivative of ${getClass.getSimpleName} undefined")
}

sealed trait SNNTerm extends NNTermBase with Ordered[SNNTerm] {
  override type TermComponent = NNTermBase

  def toChars: Iterator[Char]

  override def toString = toChars.mkString

  def +(o: SNNTerm): SNNTerm = {
    def summandlist(t: SNNTerm): IndexedSeq[SNNTerm] = t match {
      case SSum(summands) => summands
      case _ => Vector(t)
    }
    SSum((summandlist(this) ++ summandlist(o)).sorted)
  }

  def -(o: SNNTerm): SNNTerm = this + o * -1.0

  def *(o: SNNTerm): SNNTerm =
    if (this == SZERO) SZERO
    else if (this == SONE) o
    else if (o == SZERO) this
    else if (o == SONE) this
    else if (this > o) SProd(o, this) else SProd(this, o)

  def compare(o: SNNTerm): Int = {
    val (it1, it2) = (this.toChars, o.toChars)
    while (it1.hasNext && it2.hasNext) {
      val (c1, c2) = (it1.next(), it2.next())
      if (0 != c1.compareTo(c2)) return c1.compareTo(c2)
    }
    if (it1.hasNext) 1; else if (it2.hasNext) -1; else 0
  }

  protected def sumDerivatives(derivatives: Traversable[(W, SNNTerm)]): Map[W, SNNTerm] =
    derivatives.groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.reduce(_ + _))

  def wDerivative: Map[W, SNNTerm]

  def subst(s: PartialFunction[NNTerm, NNTerm]): SNNTerm
}

object SNNTerm {
  implicit def sc(value: Double): SC = SC(value)

  implicit def sc(value: Int): SC = SC(value)

  val SZERO = SC(0)
  val SONE = SC(1)
}

case class SUMMED(t: NNTerm) extends SNNTerm {
  override def toChars = "SUMMED(".toIterator ++ t.toChars ++ ")".toIterator

  override def subterms: Seq[TermComponent] = Seq(t)

  override def wDerivative: Map[W, SNNTerm] = t.wDerivative.mapValues(SUMMED)

  override def subst(s: PartialFunction[NNTerm, NNTerm]): SNNTerm = SUMMED(t.subst(s))

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), t.hashCode), 0)
}

case class SC(value: Double) extends SNNTerm {
  override def toChars = value.toString.iterator

  override def subterms: Seq[TermComponent] = Seq()

  override def wDerivative: Map[W, SNNTerm] = Map()

  override def subst(s: PartialFunction[NNTerm, NNTerm]): SNNTerm = this

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), value.hashCode), 0)
}

case class SSum(summands: IndexedSeq[SNNTerm]) extends SNNTerm {
  override def toChars = "(".toIterator ++ summands.toIterator.map(_.toChars).reduce(_ ++ " + ".toIterator ++ _) ++
    ")".toIterator

  override def subterms: Seq[TermComponent] = summands

  override def wDerivative: Map[W, SNNTerm] = sumDerivatives(summands.flatMap(_.wDerivative))

  override def subst(s: PartialFunction[NNTerm, NNTerm]): SNNTerm = SSum(summands.map(_.subst(s)))

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), seqHash(summands)), 0)
}

case class SProd(p1: SNNTerm, p2: SNNTerm) extends SNNTerm {
  override def toChars = p1.toChars ++ " * ".toIterator ++ p2.toChars

  override def subterms: Seq[TermComponent] = Seq(p1, p2)

  override def wDerivative: Map[W, SNNTerm] =
    sumDerivatives(p1.wDerivative.mapValues(_ * p2).toSeq ++ p2.wDerivative.mapValues(_ * p1).toSeq)

  override def subst(s: PartialFunction[NNTerm, NNTerm]): SNNTerm = SProd(p1.subst(s), p2.subst(s))

  override protected val theHashCode = finalizeHash(mix(stringHash(getClass.getName), mix(p1.hashCode, p2.hashCode)), 0)
}
