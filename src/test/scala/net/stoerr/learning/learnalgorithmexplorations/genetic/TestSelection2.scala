package net.stoerr.learning.learnalgorithmexplorations.genetic

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class TestSelection2 extends AnyFunSuite {

  val simpleDomain = new SelectionDomain[Double] {

    override def make: Double = 0

    override def fitness(c: Double): Double = c

    override def mutate(c: Double): Double = Random.nextDouble()

    override def crossover(c1: Double, c2: Double): Double = ???
  }

  test("approx 0") {
    val selection = new Selection(simpleDomain, 10, 0.05, 0.5)
    println(selection.best)
    (1 to 100) foreach (_ -> selection.step())
    println(selection.best)
    assert(selection.best.fitness > 0.99)
  }

}
