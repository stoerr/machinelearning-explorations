package net.stoerr.stocklearning.deepnn

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common._
import net.stoerr.stocklearning.nnfunction.ExampleForStinoNN
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.12.2014
 */
class TestDeepNN extends FunSuite {

  val eps = 1e-5

  def deriv(f: Double => Double, x: Double) = (f(x + eps) - f(x - eps)) / (2 * eps)

  def rdiff(x: Double, y: Double): Double = if (x == y) 0 else math.min(math.abs(x - y), math.abs(math.log(x / y)))

  test("Check that every parameter does something") {
    val nn = new SummingLayer(3, 2) with TanhActivation
    val gainfunc: Array[DValue] => DValue = {
      case Array(x, y, z) => x + y * z
    }
    val inputs = Array(0.1, 0.2, 0.3)
    val weights = Array.fill(nn.sizeWeights)(0.1)
    val weightFunction: (Array[Double]) => Array[Double] = nn.f(inputs) _
    for (i <- 0 until nn.sizeWeights) {
      val fprojected = weights.baseFunction(i).andThen(weightFunction)
      val f0: Array[Double] = fprojected(0)
      val feps: Array[Double] = fprojected(eps)
      assert(f0.size == 2)
      assert(feps(0) != f0(0) || feps(1) != f0(1), "Independent 0 of arg " + i)
    }
  }

  test("Check gradient one layer") {
    val nn = new SummingLayer(3, 2) with TanhActivation
    val gainfunc: Array[DValue] => DValue = {
      case Array(u, v, w, x) => u + v * w - x
    }
    val inputs = Array(0.1, 0.2, 0.3)
    val weights = Array(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
    val (_, fginfo) = nn.fg(inputs)(weights)
    for (o <- 0 until nn.sizeOutputs) {
      val outgrad: Array[Double] = Array(0.0, 0.0)
      outgrad(o) = 1.0
      val ginfo: GradInfo = fginfo(outgrad)
      val weightGradient = (0 until nn.sizeWeights).map { w =>
        val fprojected = weights.baseFunction(w).andThen(nn.f(inputs) _).andThen(_(o))
        deriv(fprojected, 0)
      }
      val inputGradient = (0 until nn.sizeInputs).map { i =>
        val fprojected = inputs.baseFunction(i).andThen(nn.f(_)(weights)).andThen(_(o))
        deriv(fprojected, 0)
      }
      assert((inputGradient, ginfo.inputGradient).zipped.map(rdiff(_, _)).reduce(math.max(_, _)) < eps)
      assert((weightGradient, ginfo.weightGradient).zipped.map(rdiff(_, _)).reduce(math.max(_, _)) < eps)
    }
  }

  test("Check gradient combined") {
    val nn = new SummingLayer(3, 4) with TanhActivation | new SummingLayer(4, 2) with TanhActivation
    val inputs = Array.fill(nn.sizeInputs)(0.1)
    val example = new ExampleForStinoNN(inputs, Array.fill(nn.sizeOutputs)(0.2))
    val weights = Array.fill(nn.sizeWeights)(0.05)
    val fgrad = nn.fgrad(example) _
    val gradient = (0 until nn.sizeWeights).map { w =>
      val fprojected = weights.baseFunction(w).andThen(fgrad(_)).andThen(_._1)
      deriv(fprojected, 0)
    }.toArray
    assert((gradient, fgrad(weights)._2).zipped.map(rdiff(_, _)).reduce(math.max(_, _)) < eps)
    assert(gradient.elem_abs.reduce(math.min(_, _)) > eps)
  }

   ignore("learning") {
    val examples = Array(
      new ExampleForStinoNN(Array(0, 0.0), Array(1, 0.0) / 2),
      new ExampleForStinoNN(Array(0, 1.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 0.0), Array(1, 1.0) / 2),
      new ExampleForStinoNN(Array(1, 1.0), Array(0, 0.0) / 2)
    )
    val nn = DeepNNLayers.basicNN(2, 5, 2)
    val f = nn.fCombined(examples) _
    val fgrad = nn.fgradCombined(examples) _
    var x = (0 until nn.sizeWeights).map(_ => math.random - 0.5).toArray
    //    var step = -0.1
    //    println("===================== GradientDescentWithWithMinimumApproximation")
    //    println(new GradientDescentWithWithMinimumApproximation(f, fgrad, 100, x, step).descent())
    //    println("===================== GradientDescentPseudoLinearNewton")
    //    println(new GradientDescentPseudoLinearNewton(f, fgrad, 100, x, step).descent())
    //    println("===================== GradientDescentMinimizeGradient")
    //    println(new GradientDescentMinimizeGradient(f, fgrad, 100, x, step).descent())
    //    println("===================== RProp")
    val (wbest, ybest, lasteps) = new RProp(f, fgrad, 200, x).descent()
    println((wbest, ybest, lasteps))
    // for (example <- examples) println(example + "\t" + nn.f(example.inputs)(wbest).toList)
    assert(ybest > 0 && ybest < eps)
  }

}
