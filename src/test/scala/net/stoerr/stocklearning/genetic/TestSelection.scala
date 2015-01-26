package net.stoerr.stocklearning.genetic

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.util.Random

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 26.01.2015
 */
class TestSelection extends FunSuite {

  val testDomain1 = new SelectionDomain[mutable.WrappedArray[Double]] {
    override def make: mutable.WrappedArray[Double] = Array.fill(3)(Random.nextDouble() * 4)

    override def crossover(c1: mutable.WrappedArray[Double], c2: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = (c1, c2).zipped.map((x1, x2) =>
      if (Random.nextInt(2) == 0) x1 else x2
    )

    override def mutate(c: mutable.WrappedArray[Double]): mutable.WrappedArray[Double] = {
      val cn = c.clone();
      val mutateIdx: Int = Random.nextInt(cn.size)
      cn(mutateIdx) = cn(mutateIdx) + Random.nextDouble() - 0.5
      cn
    }

    override def fitness(c: mutable.WrappedArray[Double]): Double = c.array match {
      case Array(x, y, z) =>
        -((x - 1) * (x - 1) * (2 * x - y) * (2 * x - y) + (z - 3) * (z - 3))
    }
  }

  test("GenericSelection") {
    val selection = new Selection(testDomain1, 100, 0.1, 0.2, 0.3)
    // println(selection.best)
    (0 until 10) foreach (_ => selection.step())
    // println(selection.best)
    assert(selection.best.fitness <= 0 && selection.best.fitness >= -0.01)
  }

}
