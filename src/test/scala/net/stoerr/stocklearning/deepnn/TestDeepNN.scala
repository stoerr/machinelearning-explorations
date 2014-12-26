package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common.{DValue, RProp}
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

  val eps = 1e-5

  def deriv(f: Double => Double, x: Double) = (f(x + eps) - f(x - eps)) / (2 * eps)

  test("Check that every parameter does something") {
    val nn = new SummingLayer(3, 2) with TanhActivation
    val gainfunc: Array[DValue] => DValue = {
      case Array(x, y, z) => x + y * z
    }
    val inputs = Array(0.1, 0.2, 0.3)
    val weights = Array.fill(nn.sizeWeights)(0.1)
    val weightFunction: (Array[Double]) => Array[Double] = nn.f(inputs) _
    for (i <- 0 until nn.sizeWeights) {
      val fprojected = weights.projectFunction(weightFunction, i)
      val f0: Array[Double] = fprojected(0)
      val feps: Array[Double] = fprojected(eps)
      assert(f0.size == 2)
      assert(feps(0) != f0(0) || feps(1) != f0(1), "Independent 0 of arg " + i)
    }
  }

  test("Check gradient") {
    val nn = new SummingLayer(3, 1) with TanhActivation
    val gainfunc: Array[DValue] => DValue = {
      case Array(u, v, w, x) => u + v * w - x
    }
    val inputs = Array(0.1, 0.2, 0.3)
    val weights = Array(0.4, 0.5, 0.6)
    val (_, fginfo) = nn.fg(inputs)(weights)
    val ginfo: GradInfo = fginfo(Array(1.0))
    for (o <- 0 until nn.sizeOutputs) {
      val weightGradient = (0 until nn.sizeWeights).map { w =>
        val fprojected = weights.projectFunction(nn.f(inputs) _, w).andThen(_(o))
        deriv(fprojected, 0)
      }
      val inputGradient = (0 until nn.sizeInputs).map { i =>
        val fprojected = inputs.projectFunction(nn.f(_)(weights), i).andThen(_(o))
        deriv(fprojected, 0)
      }
      println((inputGradient, ginfo.inputGradient).zipped.map(_ / _))
      println(ginfo.inputGradient.toList)
      println(inputGradient.toList)
      println(ginfo.weightGradient.toList)
      println(weightGradient.toList)
      assert((inputGradient, ginfo.inputGradient).zipped.map(_ / _).map(math.log).map(math.abs).reduce(math.max(_, _)) < eps)
      assert((weightGradient, ginfo.weightGradient).zipped.map(_ / _).map(math.log).map(math.abs).reduce(math.max(_, _)) < eps)
    }
  }

}
