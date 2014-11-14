package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DValue
import net.stoerr.stocklearning.common.DoubleArrayVector._
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
    val maxdifference = quotients.map(math.log).map(math.abs).reduce(math.max(_,_))
    assert(maxdifference < eps)
  }
}
