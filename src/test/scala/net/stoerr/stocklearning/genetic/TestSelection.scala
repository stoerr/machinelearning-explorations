package net.stoerr.stocklearning.genetic

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.util.Random

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 26.01.2015
 */
class TestSelection extends FunSuite {

  val testDomain = new SelectionDomain[mutable.WrappedArray[Double]] {
    override def make: mutable.WrappedArray[Double] = Array.fill(3)(Random.nextDouble() * 4)

    override def crossover(c1: mutable.WrappedArray[Double], c2: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = (c1, c2).zipped.map((x1, x2) =>
      if (Random.nextInt(2) == 0) x1 else x2
    )

    override def mutate(c: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = {
      val cn = c.clone()
      val mutateIdx: Int = Random.nextInt(cn.size)
      cn(mutateIdx) = cn(mutateIdx) + Random.nextDouble() - 0.5
      cn
    }

    override def fitness(c: mutable.WrappedArray[Double]): Double = c.array match {
      case Array(x, y, z) =>
        -((x - 1) * (x - 1) * (2 * x - y) * (2 * x - y) + (z - 3) * (z - 3))
    }
  }

  // best: n=30, fresh=0.1, mutation=0.5, xover = 0.2
  // Competitor(WrappedArray(29.0, 0.07314287729366356, 0.5755120547073116, 0.23313520936771964),-2.3032466890510694E-9)
  test("GenericSelection") {
    val selection = new Selection(testDomain, 100, 0.1, 0.4, 0.2)
    println(selection.best)
    (0 until 100) foreach (_ => selection.step())
    println(selection.best)
    assert(selection.best.fitness <= 0 && selection.best.fitness >= -0.0001)
  }

  val metaDomain = new SelectionDomain[mutable.WrappedArray[Double]] {
    override def make: mutable.WrappedArray[Double] = Array(Random.nextInt(30), Random.nextDouble() * 0.1, Random.nextDouble() * 0.6, Random.nextDouble() * 0.25)

    override def crossover(c1: mutable.WrappedArray[Double], c2: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = (c1, c2).zipped.map((x1, x2) =>
      if (Random.nextInt(2) == 0) x1 else x2
    )

    override def mutate(c: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = {
      val cn = c.clone()
      val mutateIdx: Int = Random.nextInt(cn.size)
      cn(mutateIdx) = cn(mutateIdx) * (1 + (Random.nextDouble() - 0.5) * 0.1)
      cn
    }

    override def fitness(c: mutable.WrappedArray[Double]): Double = c.array match {
      case Array(psize, freshratio, mutationratio, crossoverratio) =>
        val tries = (1 until 3).map { _ =>
          val selection = new Selection(testDomain, psize.toInt + 2, freshratio, mutationratio, crossoverratio)
          (0 until 50) foreach (_ => selection.step())
          selection.best.fitness
        }
        tries.sum / tries.length
    }
  }

  test("MetaSelection") {
    val selection = new Selection(metaDomain, 50, 0.1, 0.2, 0.3)
    println(selection.best)
    (0 until 50) foreach { i => print(i + " "); selection.step()}
    println("\n" + selection.best)
    // selection.population take 10 foreach println
    assert(selection.best.fitness <= 0 && selection.best.fitness >= -0.0000001)
  }
}
