package net.stoerr.stocklearning.common

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

}
