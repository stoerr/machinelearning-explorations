package net.stoerr.stocklearning.calculationcompiler

import java.util

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 17.01.2015
 */
sealed trait Term {

  def +(o: Term): Term = new Sum(this, o)

  def *(o: Term): Term = this match {
    case Constant(1.0) => o
    case _ => o match {
      case Constant(1.0) => this
      case _ => new Product(this, o)
    }
  }

  def eval(vars: Map[Variable, Double]): Double

  def totalDerivative: Map[Variable, Term]

  protected def sumDerivations(derivs: Traversable[Map[Variable, Term]]): Map[Variable, Term] = {
    val mapped: Map[Variable, Traversable[Term]] = derivs.flatten.groupBy(_._1).mapValues(_.map(_._2))
    mapped.mapValues(_.reduce(_ + _))
  }
}

object Term {
  implicit def toTerm(value: Double): Term = Constant(value)

  def evalOptimized(term: Term, vars: Map[Variable, Double]) = {
    import scala.collection.convert.WrapAsScala._
    val value: mutable.Map[Term, Double] = new util.IdentityHashMap[Term, Double]()
    def calculate(t: Term): Double = t match {
      case v: Variable => vars.get(v).get
      case Constant(v) => v
      case Sum(summands) => summands.map(calculate(_)).reduce(_ + _)
      case Product(f1, f2) => calculate(f1) * calculate(f2)
    }
    value.getOrElseUpdate(term, calculate(term))
  }
}

case class Variable(name: String) extends Term {
  override def toString = name

  override def totalDerivative: Map[Variable, Term] = Map(this -> Constant(1.0))

  override def eval(vars: Map[Variable, Double]): Double = vars.get(this).get // throw up when nothing's there.
}

case class Constant(value: Double) extends Term {
  override def toString = "" + value

  override def eval(vars: Map[Variable, Double]): Double = value

  override def totalDerivative: Map[Variable, Term] = Map()
}

case class Sum(summands: Seq[Term]) extends Term {
  override def toString = summands.mkString(" + ")

  def this(term1: Term, term2: Term) = this(term1 match {
    case Sum(summands) => term2 match {
      case Sum(osummands) => summands ++ osummands
      case other => summands :+ other
    }
    case other1 => term2 match {
      case Sum(osummands) => other1 +: osummands
      case other => Vector(other1, other)
    }
  })

  override def eval(vars: Map[Variable, Double]): Double = summands.map(_.eval(vars)).reduce(_ + _)

  override def totalDerivative: Map[Variable, Term] = {
    sumDerivations(summands.map(_.totalDerivative))
  }

}

case class Product(factor1: Term, factor2: Term) extends Term {
  override def toString = "(" + factor1 + ") * (" + factor2 + ")"

  override def eval(vars: Map[Variable, Double]): Double = factor1.eval(vars) * factor2.eval(vars)

  override def totalDerivative: Map[Variable, Term] = {
    val d1 = factor1.totalDerivative.mapValues(_ * factor2)
    val d2 = factor2.totalDerivative.mapValues(factor1 * _)
    sumDerivations(List(d1, d2))
  }

}
