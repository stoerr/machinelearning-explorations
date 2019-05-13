package net.stoerr.learning.gpunn.calculationcompiler

import java.util

import scala.collection.immutable.{IndexedSeq, TreeMap}
import scala.collection.mutable
import scala.language.implicitConversions

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 17.01.2015
 */
sealed trait Term {

  def +(o: Term): Term = new Sum(this, o)

  def -(o: Term): Term = new Sum(Product(this, Term.ONE), Product(o, Term.MINUSONE))

  def *(o: Term): Term = this match {
    case Constant(1.0) => o
    case _ => o match {
      case Constant(1.0) => this
      case _ => new Product(this, o)
    }
  }

  def eval(vars: Map[Variable, Double]): Double

  def totalDerivative: Map[Variable, Term]

  def asFunction(vars: Seq[Variable]): Array[Double] => Double = { x =>
    eval((vars, x).zipped.toMap)
  }

  def subterms: Seq[Term]

  def recursiveSubtermSet: Set[Term] = subterms.toSet ++ subterms.flatMap(_.recursiveSubtermSet)

  def variables: Set[Variable] = recursiveSubtermSet.filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable])

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
      case f: UnaryFunction => f.rawFunction(calculate(f.arg))
    }
    value.getOrElseUpdate(term, calculate(term))
  }

  val ONE = Constant(1.0)

  val MINUSONE = Constant(-1.0)

  def variableVector(prefix: String, size: Int): IndexedSeq[Variable] = (0 until size).map(i => Variable(prefix + ":" + i))

  def variableMatrix(prefix: String, size1: Int, size2: Int): Map[(Int, Int), Variable] =
    TreeMap((for (i <- 0 until size1; j <- 0 until size2) yield ((i, j) -> Variable(prefix + ":" + i + ":" + j))): _*)

  def tanh(arg: Term) = Tanh(arg)

  def exp(arg: Term) = Exp(arg)
}

case class Variable(name: String) extends Term {
  override def toString = name

  override def totalDerivative: Map[Variable, Term] = Map(this -> Constant(1.0))

  override def eval(vars: Map[Variable, Double]): Double = vars.get(this).get // throw up when nothing's there.

  override def subterms: Seq[Term] = Vector.empty
}

case class Constant(value: Double) extends Term {
  override def toString = "" + value

  override def eval(vars: Map[Variable, Double]): Double = value

  override def totalDerivative: Map[Variable, Term] = Map()

  override def subterms: Seq[Term] = Vector.empty
}

case class Sum(summands: Seq[Term]) extends Term {
  override def toString = "( " + summands.mkString(" + ") + ")"

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

  override def subterms: Seq[Term] = summands

}

case class Product(factor1: Term, factor2: Term) extends Term {
  override def toString = factor1 + " * " + factor2

  override def eval(vars: Map[Variable, Double]): Double = factor1.eval(vars) * factor2.eval(vars)

  override def totalDerivative: Map[Variable, Term] = {
    val d1 = factor1.totalDerivative.mapValues(factor2 * _)
    val d2 = factor2.totalDerivative.mapValues(factor1 * _)
    sumDerivations(List(d1, d2))
  }

  override def subterms: Seq[Term] = Vector(factor1, factor2)

}

trait UnaryFunction extends Term {
  val name: String
  val arg: Term

  override def toString = name + "(" + arg + ")"

  def rawFunction(arg: Double): Double

  override def eval(vars: Map[Variable, Double]): Double = rawFunction(arg.eval(vars))

  override def subterms: Seq[Term] = Vector(arg)
}

case class Tanh(arg: Term) extends UnaryFunction {
  override def totalDerivative: Map[Variable, Term] = arg.totalDerivative.mapValues(_ * (1 - this * this))

  override val name: String = "tanh"

  override def rawFunction(arg: Double): Double = math.tanh(arg)
}

case class Exp(arg: Term) extends UnaryFunction {
  override def totalDerivative: Map[Variable, Term] = arg.totalDerivative.mapValues(_ * this)

  override val name: String = "exp"

  override def rawFunction(arg: Double): Double = math.exp(arg)
}
