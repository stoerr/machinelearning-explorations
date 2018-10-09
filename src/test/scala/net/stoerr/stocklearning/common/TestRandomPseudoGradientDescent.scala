package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.deepnn.{DeepNN, DeepNNLayers}
import net.stoerr.stocklearning.util.FunctionMinimizationTestFunction
import org.scalatest.FunSuite

class TestRandomPseudoGradientDescent extends FunSuite {
  val domain = FunctionMinimizationTestFunction(20)

  test("randomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    0.until(5000).foreach { _ =>
      stepper.step()
    }
    assert( (stepper.x - domain.center).abs < 0.5)
  }

  test("orthorandomdescent") {
    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    0.until(5000).foreach { _ =>
      stepper.stepOrthogonalRandom()
    }
    assert( (stepper.x - domain.center).abs < 0.5)
  }

  val nn: DeepNN = DeepNNLayers.basicNN(2, 10, 1)
  val examples = Array(Array(0.0, 0.0) -> 0.5, Array(0.0, 1.0) -> -0.5, Array(1.0, 0.0) -> -0.5, Array(1.0, 1.0) -> 0.5)
  val startvec = Array.fill(nn.sizeWeights)(Math.random())
  def f(weights: Vec): Double = examples.map(e => nn.f(e._1)(weights)(0)).map(Math.abs).sum

  test("randomdescent ortho of NN") {
    println(startvec(0))
    println(nn)

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, startvec)
    0.until(100).foreach { _ =>
      stepper.stepOrthogonalRandom()
    }
    examples.foreach(e => println(s"${e._1.toList} : ${nn.f(e._1)(stepper.x).toList} for ${e._2}"))
  }

  test("randomdescent of NN") {
    println(startvec(0))
    println(nn)

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, startvec)
    0.until(100).foreach { _ =>
      stepper.step()
    }
    examples.foreach(e => println(s"${e._1.toList} : ${nn.f(e._1)(stepper.x).toList} for ${e._2}"))
  }

}
