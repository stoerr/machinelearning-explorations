package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.util.FunctionMinimizationTestFunction
import org.scalatest.FunSuite

class TestRandomPseudoGradientDescent extends FunSuite {

  test("randomdescent") {
    val domain = FunctionMinimizationTestFunction(20)

    val stepper = RandomPseudoGradientDescent(domain, domain.dimensions, Array.fill(domain.dimensions)(0.5))

    0.until(4000).foreach { _ =>
      stepper.step()
    }
  }

}
