package net.stoerr.stocklearning.deepnn2

import NNTerm._
import SNNTerm._

import scala.language.implicitConversions

/**
 * Definition of terms that can be used to implement neural network learning.
 * TODO: cached implementation of evaluation (parallel for SUMMED -> careful with wDerivative); do neural network.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */

sealed trait NNTermBase {
  def componentStream: Stream[NNTermBase] = this match {
    case W(_) => Stream(this)
    case I(_) => Stream(this)
    case O(_) => Stream(this)
    case C(_) => Stream(this)
    case Tanh(t) => this #:: t.componentStream
    case RLin(t) => this #:: t.componentStream
    case Step(t) => this #:: t.componentStream
    case Sqr(t) => this #:: t.componentStream
    case SoftSign(t) => this #:: t.componentStream
    case SoftSignD(t) => this #:: t.componentStream
    case Sum(summands) => this #:: summands.toStream.flatMap(_.componentStream)
    case Prod(p1, p2) => this #:: p1.componentStream #::: p2.componentStream #::: Stream.empty[NNTermBase]
    case SumProd(s) => this #:: s.toStream.flatMap(_._1.componentStream) #::: s.toStream.flatMap(_._2.componentStream)
    case SC(_) => Stream(this)
    case SSum(summands) => this #:: summands.toStream.flatMap(_.componentStream)
    case SProd(p1, p2) => this #:: p1.componentStream #::: p2.componentStream #::: Stream.empty[NNTermBase]
    case SUMMED(t) => Stream(this) ++ t.componentStream
  }

  def inputs = componentStream.filter(_.isInstanceOf[I]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[I])

  def outputs = componentStream.filter(_.isInstanceOf[O]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[O])

  def weights = componentStream.filter(_.isInstanceOf[W]).map(_.asInstanceOf[NNTerm]).toSet.toArray.sorted.map(_
    .asInstanceOf[W])
}

sealed trait NNTerm extends NNTermBase with Ordered[NNTerm] {
  def toChars: Iterator[Char]

  override def toString = toChars.mkString

  def +(o: NNTerm): NNTerm = {
    def summandlist(t: NNTerm): IndexedSeq[NNTerm] = t match {
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

  def compare(o: NNTerm): Int = {
    val (it1, it2) = (this.toChars, o.toChars)
    while (it1.hasNext && it2.hasNext) {
      val (c1, c2) = (it1.next(), it2.next())
      if (0 != c1.compareTo(c2)) return c1.compareTo(c2)
    }
    if (it1.hasNext) 1 else if (it2.hasNext) -1; else 0
  }

  private def sumDerivatives(derivatives: Traversable[(W, NNTerm)]): Map[W, NNTerm] =
    derivatives.groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.reduce(_ + _))

  def wDerivative: Map[W, NNTerm] = this match {
    case C(_) | I(_) | O(_) => Map()
    case t@W(_) => Map(t -> ONE)
    case Sum(summands) => sumDerivatives(summands.flatMap(_.wDerivative))
    case Prod(p1, p2) =>
      sumDerivatives(p1.wDerivative.mapValues(_ * p2).toSeq ++ p2.wDerivative.mapValues(_ * p1).toSeq)
    case SumProd(s) => sumDerivatives(s.flatMap(p => p._1.wDerivative.mapValues(_ * p._2)) ++
      s.flatMap(p => p._2.wDerivative.mapValues(_ * p._1)))
    case Tanh(t) => t.wDerivative.mapValues(_ * (1 - this * this))
    case RLin(t) => t.wDerivative.mapValues(_ * Step(t))
    case Sqr(t) => t.wDerivative.mapValues(_ * t * 2)
    case SoftSign(t) => t.wDerivative.mapValues(_ * SoftSignD(t))
    case Step(_) | SoftSignD(_) => sys.error("Not derivable: " + this)
  }

  def subst[T1 <: NNTerm, T2 <: NNTerm](s: PartialFunction[T1, T2]): NNTerm = this match {
    case x if s.isDefinedAt(this.asInstanceOf[T1]) => s(x.asInstanceOf[T1])
    case W(_) | I(_) | O(_) | C(_) => this
    case Tanh(t) => Tanh(t.subst(s))
    case RLin(t) => RLin(t.subst(s))
    case Step(t) => Step(t.subst(s))
    case Sqr(t) => Sqr(t.subst(s))
    case SoftSign(t) => SoftSign(t.subst(s))
    case SoftSignD(t) => SoftSignD(t.subst(s))
    case Sum(summands) => Sum(summands.map(_.subst(s)))
    case Prod(p1, p2) => Prod(p1.subst(s), p2.subst(s))
    case SumProd(prods) => SumProd(prods.map(p => (p._1.subst(s), p._2.subst(s))))
  }
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

case class W(name: String) extends NNTerm {
  override def toChars = ("W" + name).iterator
}

case class I(name: String) extends NNTerm {
  override def toChars = ("I" + name).iterator
}

case class O(name: String) extends NNTerm {
  override def toChars = ("O" + name).iterator
}

case class C(value: Double) extends NNTerm {
  override def toChars = value.toString.iterator
}

case class Sum(summands: IndexedSeq[NNTerm]) extends NNTerm {
  override def toChars = "(".toIterator ++ summands.toIterator.map(_.toChars).reduce(_ ++ " + ".toIterator ++ _) ++
    ")".toIterator

  override def hashCode = sys.error("Do not put this in a hashmap as key - that'd be seriously inefficient.")
}

case class SumProd(summands: IndexedSeq[(NNTerm, NNTerm)]) extends NNTerm {
  override def toChars = "(".toIterator ++ summands.toIterator.map(s => s._1.toChars ++ " * ".toIterator ++ s._2
    .toChars).reduce(_ ++ " + ".toIterator ++ _) ++ ")".toIterator

  override def hashCode = sys.error("Do not put this in a hashmap as key - that'd be seriously inefficient.")
}

case class Prod(p1: NNTerm, p2: NNTerm) extends NNTerm {
  override def toChars = p1.toChars ++ " * ".toIterator ++ p2.toChars
}

case class Tanh(t: NNTerm) extends NNTerm {
  override def toChars = "Tanh(".toIterator ++ t.toChars ++ ")".toIterator
}

/** Rectilinear */
case class RLin(t: NNTerm) extends NNTerm {
  override def toChars = "RLin(".toIterator ++ t.toChars ++ ")".toIterator
}

/** Derivation of rectilinear : 1 if > 0, 0 else */
case class Step(t: NNTerm) extends NNTerm {
  override def toChars = "Step(".toIterator ++ t.toChars ++ ")".toIterator
}

case class Sqr(t: NNTerm) extends NNTerm {
  override def toChars = "Sqr(".toIterator ++ t.toChars ++ ")".toIterator
}

case class SoftSign(t: NNTerm) extends NNTerm {
  override def toChars = "SoftSign(".toIterator ++ t.toChars ++ ")".toIterator
}

/** Derivation of SoftSign */
case class SoftSignD(t: NNTerm) extends NNTerm {
  override def toChars = "SoftSignD(".toIterator ++ t.toChars ++ ")".toIterator
}

sealed trait SNNTerm extends NNTermBase with Ordered[SNNTerm] {
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

  private def sumDerivatives(derivatives: Traversable[(W, SNNTerm)]): Map[W, SNNTerm] =
    derivatives.groupBy(_._1).mapValues(_.map(_._2)).mapValues(_.reduce(_ + _))

  def wDerivative: Map[W, SNNTerm] = this match {
    case SC(_) => Map()
    case SSum(summands) => sumDerivatives(summands.flatMap(_.wDerivative))
    case SProd(p1, p2) =>
      sumDerivatives(p1.wDerivative.mapValues(_ * p2).toSeq ++ p2.wDerivative.mapValues(_ * p1).toSeq)
    case SUMMED(t) => t.wDerivative.mapValues(SUMMED)
  }

  def subst[T1 <: NNTerm, T2 <: NNTerm](s: PartialFunction[T1, T2]): SNNTerm = this match {
    case SC(c) => this
    case SSum(summands) => SSum(summands.map(_.subst(s)))
    case SProd(p1, p2) => SProd(p1.subst(s), p2.subst(s))
    case SUMMED(t) => SUMMED(t.subst(s))
  }
}

object SNNTerm {
  implicit def sc(value: Double): SC = SC(value)

  implicit def sc(value: Int): SC = SC(value)

  val SZERO = SC(0)
  val SONE = SC(1)
}

case class SUMMED(t: NNTerm) extends SNNTerm {
  override def toChars = "SUMMED(".toIterator ++ t.toChars ++ ")".toIterator
}

case class SC(value: Double) extends SNNTerm {
  override def toChars = value.toString.iterator
}

case class SSum(summands: IndexedSeq[SNNTerm]) extends SNNTerm {
  override def toChars = "(".toIterator ++ summands.toIterator.map(_.toChars).reduce(_ ++ " + ".toIterator ++ _) ++
    ")".toIterator
}

case class SProd(p1: SNNTerm, p2: SNNTerm) extends SNNTerm {
  override def toChars = p1.toChars ++ " * ".toIterator ++ p2.toChars
}
