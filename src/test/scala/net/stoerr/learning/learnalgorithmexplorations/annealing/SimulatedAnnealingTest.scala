package net.stoerr.learning.learnalgorithmexplorations.annealing

import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector.Vec
import org.scalatest.FunSuite

class SimulatedAnnealingTest extends FunSuite {

  // test("testMutate") {(-1.1).until(1.11,0.1).foreach(x => println(s"$x, ${mutateFunc(0.6, x)}"))}

  test("anneal") {
    val fitness : Vec => Double = {
      case Array(x: Double, y: Double) => 1.0 - (x - 0.6) * (x - 0.6) - (y - 0.7) * (y - 0.7)
    }
    val anneal = new SimulatedAnnealing(2, fitness, SimulatedAnnealing.stepWidthLog(20))
    val result = anneal.stepping()
    // println(anneal.population.mkString("|"))
    println(result.item.toList)
    println(result.fitness)
    assert(result.fitness > 0.99999)
    val v: Vec = result.item
    assert(v(0) - 0.6 < 0.001)
    assert(v(1) - 0.7 < 0.001)
  }

}
