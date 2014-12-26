package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common.RProp
import net.stoerr.stocklearning.nnfunction.ExampleForStinoNN
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.12.2014
 */
class TestDeepNN extends FunSuite {
  test("learning") {
    val examples = Array(
      new ExampleForStinoNN(Array(0, 0.0), Array(1, 0.0) / 2),
      new ExampleForStinoNN(Array(0, 1.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 0.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 1.0), Array(0, 0.0) / 2)
    )
    val nn = DeepNNLayers.twoLayerNN(2, 3, 2)
    val f = nn.fCombined(examples) _
    val fgrad = nn.fgradCombined(examples) _
    var x = (0 until nn.sizeWeights).map(_ => math.random - 0.5).toArray
    var eps = -0.1
    //    println("===================== GradientDescentWithWithMinimumApproximation")
    //    println(new GradientDescentWithWithMinimumApproximation(f, fgrad, 100, x, eps).descent())
    //    println("===================== GradientDescentPseudoLinearNewton")
    //    println(new GradientDescentPseudoLinearNewton(f, fgrad, 100, x, eps).descent())
    //    println("===================== GradientDescentMinimizeGradient")
    //    println(new GradientDescentMinimizeGradient(f, fgrad, 100, x, eps).descent())
    println("===================== RProp")
    println(new RProp(f, fgrad, 200, x).descent())
  }

}
