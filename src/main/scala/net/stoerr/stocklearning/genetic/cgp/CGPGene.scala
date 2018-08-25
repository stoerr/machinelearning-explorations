package net.stoerr.stocklearning.genetic.cgp

import java.lang.Math._

import CGPGene.fieldHasParameters

import scala.collection.mutable
import scala.util.Random

object CGPGene {
  val fieldHasParameters = 4
}

/** CGP "individual". Parameters are all in [0,1). There are four parameters per item, x, y, p, f.
  * The last numout parameters signify the outputs. */
case class CGPGene(param: Array[Double], numin: Int, numout: Int) {
  def this(numcalc: Int, numin: Int, numout: Int) = this(
    0.until(numcalc * fieldHasParameters).map(_ => Random.nextDouble()).toArray, numin, numout
  )

  protected val parms: Int = param.length


  def mutate(): CGPGene = {
    val paramCopy = param.clone()
    paramCopy(Random.nextInt(paramCopy.length)) = Random.nextDouble()
    this.copy(param = paramCopy)
  }

  def calculate(in: Array[Double]): Array[Double] = {
    val calc = new Calculator(in)
    0.until(numout).map(o => calc.calculate(param(parms - numout + o), parms - numout)).toArray
  }

  def formula(): String = {
    val stringBuilder = new mutable.StringBuilder();
    val calc = new Calculator(Array.fill(numin)(0))
    0.until(numout).foreach { o =>
      calc.calculate(param(parms - numout + o), parms - numout)
      stringBuilder.append("o" + o + " = " + calc.symMap(param(parms - numout + o), parms - numout) + "\n")
    }
    calc.appendFormulas(stringBuilder, parms - numout)
    stringBuilder.toString()
  }

  protected class Calculator(in: Array[Double]) {

    protected val calculated: mutable.Map[Int, Double] = mutable.Map[Int, Double]()

    /** Left means input, Right means cell */
    protected def map(d: Double, maxl: Int): Either[Int, Int] = {
      val idx = Math.floor(d * (maxl + numin) - numin).toInt
      if (idx < 0) Left(idx + numin) else Right(idx / fieldHasParameters * fieldHasParameters)
    }

    def calculate(field: Double, maxIdx: Int): Double = map(field, maxIdx) match {
      case Left(input) => in(input)
      case Right(idx) => calculated.getOrElseUpdate(idx, {
        val function = CGPFunction(param(idx))
        val x = calculate(param(idx + 1), idx)
        val y = calculate(param(idx + 2), idx)
        function(x, y, param(idx + 3))
      }
      )
    }

    def symMap(d: Double, maxl: Int): String = map(d, maxl) match {
      case Left(input) => "in" + input
      case Right(idx) => "c" + idx
    }

    def appendFormulas(stringBuilder: StringBuilder, maxIdx: Int): Unit = {
      0.to(maxIdx - 1, fieldHasParameters).reverse.filter(calculated.contains) foreach { idx =>
        val function = CGPFunction(param(idx))
        stringBuilder.append(
          "c" + (idx / fieldHasParameters) + " = " +
            function.toString + "(" + symMap(param(idx + 1), idx) + ", " + symMap(param(idx + 2), idx) + ", " + param(idx + 3) + ")\n"
        )
      }
    }
  }

}

sealed trait CGPFunction {
  def apply(x: => Double, y: => Double, p: Double): Double

  protected def expandP(p: Double): Double = 2 * p - 1
}

object CGPFunction {
  val values = List(Add, AMinus, Mult, Cmult, Inv, Abs, Sqrt, CPow, YPow, ExpX, Sin, SqrtXY, Max, Min)

  def apply(f: Double): CGPFunction = values(Math.floor(f * values.size).toInt)
}

case object Add extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = (x + y) / 2
}

case object AMinus extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = abs(x - y) / 2
}

case object Mult extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = x * y
}

case object Cmult extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * x
}

case object Inv extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = if (x != 0) 1 / x else 0
}

case object Abs extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = abs(x)
}

case object Sqrt extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = sqrt(abs(x))
}

case object CPow extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = pow(abs(x), p)
}

case object YPow extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = pow(abs(x), abs(y))
}

case object ExpX extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = (exp(x) - 1) / (E - 1)
}

case object Sin extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = sin(2 * PI * p * x)
}

case object SqrtXY extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = sqrt(x * x + y * y) / sqrt(2)
}

case object Max extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = max(x, y)
}

case object Min extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = min(x, y)
}

case object Log extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = log(x)
}

