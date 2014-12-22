package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common._
import org.scalatest.FunSuite

import scala.collection.immutable.IndexedSeq

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 13.11.2014
 */
class TestNNasFunction extends FunSuite {

  val eps = 1e-7

  def deriv(f: Double => Double, x: Double) = (f(x + eps) - f(x - eps)) / (2 * eps)

  test("Check that every parameter does something") {
    val nn = new NNasFunction(3, 7, 3)
    val gainfunc: Array[DValue] => DValue = {
      case Array(x, y, z) => x + y * z
    }
    val ex = new ExampleWithDValueFunction(Array(0.2, 0.2, 0.2), gainfunc)
    val base = Array.fill(nn.dimension)(0.1)
    val weightFunction = nn.weightFunction(ex)
    for (i <- 0 until nn.dimension) {
      val fprojected = base.projectFunction(weightFunction, i)
      val f0: Double = fprojected(0)
      val feps: Double = fprojected(eps)
      assert(feps != f0, "Independent of arg " + i)
    }
  }

  test("Check gradient") {
    val nn = new NNasFunction(3, 7, 4)
    val gainfunc: Array[DValue] => DValue = {
      case Array(u, v, w, x) => u + v * w - x
    }
    val ex = new ExampleWithDValueFunction(Array.fill(nn.inputSize)(0.5), gainfunc)
    val f: (Array[Double]) => Double = nn.weightFunction(ex)
    val base = Array.fill(nn.dimension)(0.1)
    val weightFunction = nn.weightFunction(ex)
    val realgradient = 0.until(nn.dimension).map { i =>
      val fprojected = base.projectFunction(weightFunction, i)
      deriv(fprojected, 0)
    }
    val (value, gradient) = nn.weightFunctionWithGradient(ex)(base)
    val quotients: IndexedSeq[Double] = (realgradient, gradient).zipped.map(_ / _)
    val maxdifference = quotients.map(math.log).map(math.abs).reduce(math.max(_, _))
    assert(maxdifference < eps)
  }


  test("learning") {
    val examples = Array(
      new ExampleForStinoNN(Array(0, 0.0), Array(1, 0.0) / 2),
      new ExampleForStinoNN(Array(0, 1.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 0.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 1.0), Array(0, 0.0) / 2)
    )
    val nn = new NNasFunction(2, 3, 2)
    val f = nn.joinedWeightFunction(examples)
    val fgrad = nn.joinedWeightFunctionWithGradient(examples)
    var x = (0 until nn.dimension).map(_ => math.random - 0.5).toArray
    var eps = -0.1
    println("===================== GradientDescentWithWithMinimumApproximation")
    println(new GradientDescentWithWithMinimumApproximation(f, fgrad, 100, x, eps).descent())
    println("===================== GradientDescentPseudoLinearNewton")
    println(new GradientDescentPseudoLinearNewton(f, fgrad, 100, x, eps).descent())
    println("===================== GradientDescentMinimizeGradient")
    println(new GradientDescentMinimizeGradient(f, fgrad, 100, x, eps).descent())
    println("===================== RProp")
    println(new RProp(f, fgrad, 200, x).descent())
    //    for (i <- 0 until 100) {
    //      val (y,grad) = fgrad(x)
    //      val directedFunc = x.directionalFunction(f, grad)
    //      eps = GradientDescent.approximateMinimum(y, directedFunc, eps)
    //      println(y + "\t" + eps)
    //      x = x + grad*eps
    //    }
    //    println(f(x))
    // println(x.mkString(","))
    // println(nn.toString(x))
  }

}
