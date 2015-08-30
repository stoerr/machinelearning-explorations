package net.stoerr.stocklearning.deepnn2

import NNTerm._
import SNNTerm._

import scala.language.implicitConversions

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */
sealed trait NNTerm extends Ordered[NNTerm] {
  def toChars: Iterator[Char]

  override def toString = toChars.mkString

  def +(o: NNTerm): NNTerm = {
    def summandlist(t: NNTerm): IndexedSeq[NNTerm] = t match {
      case Sum(summands) => summands
      case _ => Vector(t)
    }
    Sum((summandlist(this) ++ summandlist(o)).sorted)
  }

  def *(o: NNTerm): NNTerm =
    if (this == ZERO) ZERO
    else if (this == ONE) o
    else if (o == ZERO) this
    else if (o == ONE) this
    else if (this > o) Prod(o, this) else Prod(this, o)

  def compare(o: NNTerm): Int = {
    val (it1, it2) = (this.toChars, o.toChars)
    while (it1.hasNext && it2.hasNext) {
      val (c1, c2) = (it1.next(), it2.next())
      if (0 != c1.compareTo(c2)) return c1.compareTo(c2)
    }
    if (it1.hasNext) 1 else if (it2.hasNext) -1; else 0
  }

  def componentStream: Stream[NNTerm] = this match {
    case W(_) => Stream(this)
    case I(_) => Stream(this)
    case C(_) => Stream(this)
    case Sum(summands) => this #:: summands.toStream.flatMap(_.componentStream)
    case Prod(p1, p2) => this #:: p1 #:: p2 #:: Stream.empty
  }

  def eval(valuation: PartialFunction[NNTerm, Double]): Double = this match {
    case C(v) => v
    case v if valuation.isDefinedAt(v) => valuation(v)
    case Sum(summands) => summands.map(_.eval(valuation)).reduce(_ + _)
    case Prod(p1, p2) => p1.eval(valuation) * p2.eval(valuation)
  }
}

object NNTerm {
  implicit def c(value: Double): C = C(value)

  val ZERO = C(0)
  val ONE = C(1)
}

case class W(name: String) extends NNTerm {
  override def toChars = ("W" + name).iterator
}

case class I(name: String) extends NNTerm {
  override def toChars = ("I" + name).iterator
}

case class C(value: Double) extends NNTerm {
  override def toChars = value.toString.iterator
}

case class Sum(summands: IndexedSeq[NNTerm]) extends NNTerm {
  override def toChars = "(".toIterator ++ summands.toIterator.map(_.toChars).reduce(_ ++ " + ".toIterator ++ _) ++
    ")".toIterator
}

case class Prod(p1: NNTerm, p2: NNTerm) extends NNTerm {
  override def toChars = p1.toChars ++ " * ".toIterator ++ p2.toChars
}

sealed trait SNNTerm extends Ordered[SNNTerm] {
  def toChars: Iterator[Char]

  override def toString = toChars.mkString

  def +(o: SNNTerm): SNNTerm = {
    def summandlist(t: SNNTerm): IndexedSeq[SNNTerm] = t match {
      case SSum(summands) => summands
      case _ => Vector(t)
    }
    SSum((summandlist(this) ++ summandlist(o)).sorted)
  }

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

  def componentStream: Stream[Either[SNNTerm, NNTerm]] = this match {
    case SC(_) => Stream(Left(this))
    case SSum(summands) => Left(this) #:: summands.toStream.flatMap(_.componentStream)
    case SProd(p1, p2) => Left(p1) #:: Left(p2) #:: Stream.empty
    case SUMMED(t) => Stream(Left(this)) ++ t.componentStream.map(Right(_))
  }
}

object SNNTerm {
  implicit def sc(value: Double): SC = SC(value)

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
