package net.stoerr.stocklearning.calculationcompiler

import net.stoerr.stocklearning.common.DoubleArrayVector._
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.01.2015
 */
class TestTerm extends FunSuite {

  def assertAlmostEqual(x: Double, y: Double): Unit = assert(math.abs(x - y) < eps)

  test("just try it") {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val f = (x + y * 2.0 + z) * z
    println(f)
    val deriv: Map[Variable, Term] = f.totalDerivative
    println(deriv)
    val v = Array(0.2, 0.3, 0.5)
    val expectedFunc: Double = (0.2 + 0.3 * 2.0 + 0.5) * 0.5
    val variables: List[Variable] = List(x, y, z)
    val func: (Array[Double]) => Double = f.asFunction(variables)
    println(func(v))
    assertAlmostEqual(func(v), expectedFunc)
    val grad: Array[Double] = gradient(func, v)
    val gradSymbolic = variables.map(deriv(_).asFunction(variables)(v))
    println(grad.toList)
    println(gradSymbolic.toList)
    (grad, gradSymbolic).zipped.foreach(assertAlmostEqual(_, _))

    val funcOpt = { x: Array[Double] => Term.evalOptimized(f, ((variables, x).zipped.toMap))}
    assertAlmostEqual(funcOpt(v), expectedFunc)

    val compiler = new CalculationTermCompiler
    val compiledFunc = compiler.toFunction(variables, f)
    assertAlmostEqual(compiledFunc(v), func(v))
  }

}
