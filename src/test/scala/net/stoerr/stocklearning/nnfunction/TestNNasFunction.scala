package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DValue
import org.scalatest.FunSuite

import net.stoerr.stocklearning.common.DoubleArrayVector._

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 13.11.2014
 */
class TestNNasFunction extends FunSuite {

  val eps = 1e-7

  test("Check that every parameter does something") {
    val nn = new NNasFunction(3, 7, 3)
    val gainfunc: Array[DValue] => DValue = {
      case Array(x, y, z) => x + y * z
    }
    val ex = new ExampleWithDValueFunction(Array(0.2, 0.2, 0.2), gainfunc)
    val f: (Array[Double]) => Double = nn.weightFunction(ex)
    val base = Array.fill(nn.dimension)(0.1)
    val weightFunction = nn.weightFunction(ex)
    for (i <- 0 until nn.dimension) {
      val fprojected = base.projectFunction(weightFunction, i)
      val f0: Double = fprojected(0)
      val feps: Double = fprojected(eps)
      assert(feps != f0, "Independent of arg " + i)
    }
  }
}

