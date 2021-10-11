package net.stoerr.learning.learnalgorithmexplorations.annealing

import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector.Vec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class SimulatedAnnealingTest extends AnyFunSuite {

  // test("testMutate") {(-1.1).until(1.11,0.1).foreach(x => println(s"$x, ${mutateFunc(0.6, x)}"))}

  test("anneal") {
    val fitness: Vec => Double = {
      case Array(x: Double, y: Double) => 1.0 - (x - 0.6) * (x - 0.6) - (y - 0.7) * (y - 0.7)
    }
    var anneal = new AdaptiveSimulatedAnnealing(2, fitness)
    anneal = anneal.findOptimum(100)
    println(anneal.stepstaken)
    val result = anneal.item
    // println(anneal.population.mkString("|"))
    println(result.vec.toList)
    println(result.fitness)
    result.fitness should be > 0.99999
    val v: Vec = result.vec
    Math.abs(v(0) - 0.6) should be < 0.001
    Math.abs(v(1) - 0.7) should be < 0.001
  }

  test("populationanneal") {
    val fitness: Vec => Double = {
      case Array(x: Double, y: Double) => 1.0 - (x - 0.6) * (x - 0.6) - (y - 0.7) * (y - 0.7)
    }
    var anneal = new SimulatedAnnealingEvolution(2, fitness, 5, 0.1)
    val result = anneal.findOptimum(100).item
    // println(anneal.population.mkString("|"))
    println(result.vec.toList)
    println(result.fitness)
    result.fitness should be > 0.99999
    val v: Vec = result.vec
    Math.abs(v(0) - 0.6) should be < 0.001
    Math.abs(v(1) - 0.7) should be < 0.001
  }


}
