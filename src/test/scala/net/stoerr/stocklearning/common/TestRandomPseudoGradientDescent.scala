package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector.Vec
import net.stoerr.stocklearning.deepnn.{DeepNN, DeepNNLayers}
import net.stoerr.stocklearning.util.FunctionMinimizationTestFunction
import org.scalatest.FunSuite

class TestRandomPseudoGradientDescent extends FunSuite {
  val domain = FunctionMinimizationTestFunction(20)

  test("randomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    0.until(2000).foreach { _ =>
      stepper.step()
    }
  }

  test("orthorandomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    0.until(2000).foreach { _ =>
      stepper.stepOrthogonalRandom()
    }
  }

  test("randomdescent of NN") {
    val nn: DeepNN = DeepNNLayers.basicNN(2, 5, 1)
    println(nn)
    val examples = Array(Array(0.0, 0.0) -> 0.5, Array(0.0, 1.0) -> -0.5, Array(1.0, 0.0) -> -0.5, Array(1.0, 1.0) -> 0.5)

    def f(weights: Vec): Double = examples.map(e => nn.f(e._1)(weights)(0)).map(Math.abs).sum

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, Array.fill(nn.sizeWeights)(Math.random()))
    0.until(100).foreach { _ =>
      stepper.step()
    }
    examples.foreach(e => println(s"${e._1.toList} : ${nn.f(e._1)(stepper.x).toList}"))
  }

}
