package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.deepnn.{DeepNN, DeepNNLayers}
import net.stoerr.stocklearning.util.FunctionMinimizationTestFunction
import org.scalatest.FunSuite

class TestRandomPseudoGradientDescent extends FunSuite {
  val domain = FunctionMinimizationTestFunction(20)

  val nn: DeepNN = DeepNNLayers.basicNN(2, 7, 7, 1)
  val examples: Array[(Array[Double], Double)] = Array(Array(0.0, 0.0) -> 0.5, Array(0.0, 1.0) -> -0.5, Array(1.0, 0.0) -> -0.5, Array(1.0, 1.0) -> 0.5)
  val startvec: Array[Double] = Array.fill(nn.sizeWeights)(Math.random())

  def f(weights: Vec): Double = examples.map(e => nn.f(e._1)(weights)(0) - e._2).map(x => x * x).sum

  test("randomdescent of NN") {
    println(startvec(0))
    println(nn)

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, startvec)
    for (i <- 0 until 1000 if i < 100 || stepper.laststep.abs > 1e-8 && f(stepper.x) > 1e-4 ) {
      stepper.step()
    }
    examples.foreach(e => println(s"${e._1.toList} : ${nn.f(e._1)(stepper.x).toList} for ${e._2}"))
  }

  test("randomdescent ortho of NN") {
    println(startvec(0))
    println(nn)

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, startvec)
    for (i <- 0 until 1000 if i < 100 || stepper.laststep.abs > 1e-8 && f(stepper.x) > 1e-4) {
      stepper.stepOrthogonalRandom()
    }
    examples.foreach(e => println(s"${e._1.toList} : ${nn.f(e._1)(stepper.x).toList} for ${e._2}"))
  }

  test("randomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    for (i <- 0 until 1000 if (stepper.x - domain.center).abs > 0.1) {
      stepper.step()
    }
    assert((stepper.x - domain.center).abs < 0.5)
  }

  test("orthorandomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    for (i <- 0 until 1000 if (stepper.x - domain.center).abs > 0.1) {
      stepper.stepOrthogonalRandom()
    }
    assert((stepper.x - domain.center).abs < 0.5)
  }

}
