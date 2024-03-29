package net.stoerr.learning.learnalgorithmexplorations.common

import DValue._
import net.stoerr.learning.learnalgorithmexplorations.nnfunction.{FunctionMinimizationTestDomain, FunctionMinimizationTestFunction}
import net.stoerr.learning.learnalgorithmexplorations.nnfunction.FunctionMinimizationTestDomain
import org.scalatest.funsuite.AnyFunSuite

/**
  * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
  * @since 14.11.2014
  */
class TestGradientDescent extends AnyFunSuite {

  val eps = 1e-7

  test("descentWithMinimumApproximation") {
    def fd(args: Array[DValue]) = {
      val dif = args(0) - args(1)
      dif.cosh + (args(0) * args(0) + args(1) * args(1))
    }

    val f: Array[Double] => Double = asDoubleFunction(fd)
    val fgrad: Array[Double] => (Double, Array[Double]) = asDoubleFunctionWithGradient(fd)
    println("GradientDescentWithWithMinimumApproximation");
    {
      val (x, y, dy) = new GradientDescentWithWithMinimumApproximation(f, fgrad, 25, Array(1001.0, 1000.0)).descent()
      println(x.toList, y, dy)
    }
    println("GradientDescentPseudoLinearNewton");
    {
      val (x, y, dy) = new GradientDescentPseudoLinearNewton(f, fgrad, 25, Array(101.0, 100.0)).descent()
      println(x.toList, y, dy)
    }
    println("GradientDescentMinimizeGradient");
    {
      val (x, y, dy) = new GradientDescentMinimizeGradient(f, fgrad, 25, Array(101.0, 100.0)).descent()
      println(x.toList, y, dy)
    }
  }

  test("Find Minimum") {
    for (m <- Array(0.3, 1.2, 4.7)) {
      def f(x: Double) = 2.6 * (x - m) * (x - m)

      for ((x0, x1, x2) <- Array((2, 5, 7), (1, 2, 3))) {
        val min = NumericalMinimumFinder.interpolatedMinimum(x0, f(x0), x1, f(x1), x2, f(x2))
        assert(math.abs(min - m) < eps)
      }
    }
  }

  test("approximateMinimum") {
    def f(x: Double) = -math.sin(x) + 0.3743

    for (eps <- Array(0.01, 1, 1.5, 2, 4)) {
      val minimum: Double = GradientDescent.approximateMinimum(f(0), f, eps)
      assert(math.abs(minimum - math.Pi / 2) < 0.2)
    }

    def g(x: Double) = math.cosh(x - 1)

    val minimum = GradientDescent.approximateMinimum(g(0), g, 10)
    assert(math.abs(minimum - 1) < 0.1)
  }

  test("FunctionMinimizationTestFunction") {
    val dim = 20
    val domain = FunctionMinimizationTestFunction(dim)
    val fgrad: Array[Double] => (Double, Array[Double]) = v => (domain(v), DoubleArrayVector.gradient(domain)(v))
    val (x, y, dy) = new GradientDescentWithWithMinimumApproximation(domain, fgrad, 100, Array.fill(dim)(0.5)).descent()
    println(x.toList, y, dy)
    assert(y >= 0)
    assert(y <= 0.1)
  }

  test("FunctionMinimizationTestDomain") {
    val dim = 10
    val domain = FunctionMinimizationTestDomain(dim)
    val fitness = domain evaluateStatistics { domain =>
      val fgrad: Array[Double] => (Double, Array[Double]) = v => (domain(v), DoubleArrayVector.gradient(domain)(v))
      val (x, y, dy) = new GradientDescentWithWithMinimumApproximation(domain, fgrad, 100, Array.fill(dim)(0.5)).descent()
      // println(x.toList, y, dy)
      println(y)
      x
    }
    println(fitness)
    assert(fitness.min > 0)
    assert(fitness.mean <= 0.1)
  }

}
