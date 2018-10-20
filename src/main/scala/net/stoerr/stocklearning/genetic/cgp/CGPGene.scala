package net.stoerr.stocklearning.genetic.cgp

import java.lang.Math._

import net.stoerr.stocklearning.genetic.cgp.CGPGene._

import scala.collection.mutable
import scala.util.Random

object CGPGene {
  val parametersPerField = 4
  val outputMutationProbability = 0.06
}

/** CGP "individual". Parameters are all in [0,1). There are four parameters per item, x, y, p, f.
  * The last numout parameters signify the outputs.
  *
  * @param parameters : calculations ( fieldHasParameters values each ) and then numout output selectors */
case class CGPGene(numin: Int, numcalc: Int, numout: Int, fieldParam: Array[Double], outParam: Array[Double]) {
  def this(numin: Int, numcalc: Int, numout: Int) = this(
    numin, numcalc, numout,
    0.until(numcalc * parametersPerField).map(_ => Random.nextDouble()).toArray,
    0.until(numout).map(_ => Random.nextDouble()).toArray
  )

  assert(fieldParam.size == numcalc * parametersPerField)
  assert(outParam.size == numout)

  def mutateRandom(): CGPGene = {
    val paramCopy = fieldParam.clone()
    val outCopy = outParam.clone()
    paramCopy(Random.nextInt(paramCopy.length)) = Random.nextDouble()
    if (Random.nextDouble() < outputMutationProbability)
      outCopy(Random.nextInt(numout)) = Random.nextDouble()
    this.copy(fieldParam = paramCopy, outParam = outCopy)
  }

  def mutateUntilVisible(): CGPGene = {
    val calc = new Calculator(Array.fill(numin)(1 / PI))
    0.until(numout).foreach(o => calc.calculate(outParam(o)))
    val paramCopy = fieldParam.clone()
    val outCopy = outParam.clone()
    paramCopy(Random.nextInt(paramCopy.length)) = Random.nextDouble()
    if (Random.nextDouble() < outputMutationProbability)
      outCopy(Random.nextInt(numout)) = Random.nextDouble()
    else if (calc.cached.nonEmpty) {
      var visibleMutation = false
      while (!visibleMutation) {
        val nextmutation = Random.nextInt(numcalc * parametersPerField)
        paramCopy(nextmutation) = Random.nextDouble()
        visibleMutation = calc.cached.contains(nextmutation / parametersPerField)
      }
    }
    this.copy(fieldParam = paramCopy, outParam = outCopy)
  }

  def calculate(in: Array[Double]): Array[Double] = {
    assert(in.length == numin)
    val calc = new Calculator(in)
    0.until(numout).map(o => calc.calculate(outParam(o))).toArray
  }

  def serializedFull: String = s"CGPGene($numin,$numcalc,$numout, Array(${fieldParam.mkString(",")}), Array(${outParam.mkString(",")}))"

  /** minimum representation of output function. */
  def serialized: String = {
    val calc = new Calculator(Array.fill(numin)(1 / PI))
    0.until(numout).foreach(o => calc.calculate(outParam(o)))
    val cleanedFieldParams = fieldParam.zipWithIndex
      .map(p => if (calc.cached.contains(p._2 / parametersPerField)) p else (0.0, p._2)).map(_._1)
    CGPGene(numin, numcalc, numout, cleanedFieldParams, outParam).serializedFull
  }

  def formula: String = {
    val stringBuilder = new mutable.StringBuilder()
    val calc = new Calculator(Array.fill(numin)(0))
    0.until(numout).foreach { o =>
      calc.calculate(outParam(o))
      stringBuilder.append("o" + o + " = " + calc.symMap(outParam(o)) + "\n")
    }
    calc.appendFormulas(stringBuilder)
    stringBuilder.toString()
  }

  private class Calculator(in: Array[Double]) {

    /** Maps field to it's result, if already calculated. */
    val cached: mutable.Map[Int, Double] = mutable.Map[Int, Double]()

    /** Left means input number, Right means field. maxFld = the max. field (excl.) */
    protected def map(d: Double, maxFld: Int): Either[Int, Int] = {
      val idx = Math.floor(d * (maxFld + numin) - numin).toInt
      assert(idx < maxFld)
      assert(idx + numin >= 0)
      if (idx < 0) Left(idx + numin) else Right(idx)
    }

    def calculate(field: Double, maxIdx: Int = numcalc): Double = map(field, maxIdx) match {
      case Left(input) => in(input)
      case Right(idx) => cached.getOrElseUpdate(idx, {
        val fieldIdx = idx * parametersPerField
        val function = CGPFunction(fieldParam(fieldIdx))
        val x = calculate(fieldParam(fieldIdx + 1), idx)
        val y = calculate(fieldParam(fieldIdx + 2), idx)
        function(x, y, fieldParam(fieldIdx + 3))
      }
      )
    }

    def symMap(d: Double, maxFld: Int = numcalc): String = map(d, maxFld) match {
      case Left(input) => "in" + input
      case Right(idx) => "c" + idx
    }

    def appendFormulas(stringBuilder: StringBuilder, maxIdx: Int = numcalc): Unit = {
      0.to(maxIdx - 1).reverse.filter(cached.contains) foreach { idx =>
        val fieldIdx = idx * parametersPerField
        val function = CGPFunction(fieldParam(fieldIdx))
        stringBuilder.append(
          s"c$idx = $function(${symMap(fieldParam(fieldIdx + 1), idx)}, ${
            symMap(fieldParam(fieldIdx + 2), idx)
          }, ${fieldParam(fieldIdx + 3)})\n"
        )
      }
    }
  }

}

sealed trait CGPFunction {
  def apply(x: => Double, y: => Double, p: Double): Double

  protected def expandP(p: Double): Double = 4 * p - 2
}

object CGPFunction {
  val values = List(Add, AMinus, Mult, Cmult, Inv, Abs, Sqrt, CPow, YPow, ExpX, Sin, SqrtXY, Max, Min)

  def apply(f: Double): CGPFunction = values(Math.floor(f * values.size).toInt)
}

case object Add extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * (x + y) / 2
}

case object AMinus extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * abs(x - y) / 2
}

case object Mult extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * x * y
}

case object Cmult extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * x
}

case object Inv extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * (if (x != 0) 1 / x else 0)
}

case object Abs extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * abs(x)
}

case object Sqrt extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * sqrt(abs(x))
}

case object CPow extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = pow(abs(x), p)
}

case object YPow extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * pow(abs(x), abs(y))
}

case object ExpX extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * (exp(x) - 1) / (E - 1)
}

case object Sin extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = sin(2 * PI * p * x)
}

case object SqrtXY extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * sqrt(x * x + y * y) / sqrt(2)
}

case object Max extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * max(x, y)
}

case object Min extends CGPFunction {
  override def apply(x: => Double, y: => Double, p: Double): Double = expandP(p) * min(x, y)
}
