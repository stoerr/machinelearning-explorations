package net.stoerr.stocklearning.nnfunction

import net.stoerr.stocklearning.common.DValue
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 13.11.2014
 */
class TestNNasFunction extends FunSuite {

  test("Check that every parameter does something") {
    val nn = new NNasFunction(3, 7, 3)
    val gainfunc: Array[DValue] => DValue = {
      case Array(x, y, z) => x + y * z
    }
    // XXX
    val ex = new ExampleWithDValueFunction(Array(0.1, 0.23, 0.618), gainfunc)

  }
}

