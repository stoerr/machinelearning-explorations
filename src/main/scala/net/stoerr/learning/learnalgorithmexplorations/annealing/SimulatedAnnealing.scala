package net.stoerr.learning.learnalgorithmexplorations.annealing

import java.util.concurrent.TimeUnit

import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector
import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector._

import scala.util.Random

/** Takes an optimization problem with a set of parameters between 0 and 1 and tries to solve it using
  * random steps which decrease in size over time.
  *
  * @param stepWidth a function that gives the step width depending on the round number. If <= 0 we stop.
  */
class SimulatedAnnealing(dim: Int, fitness: Vec => Double, stepWidth: Int => Double, populationsize: Int = 10)
  extends Evolution[Vec](populationsize, fitness) {
  override protected def makeRandomItem(): Vec = 0.until(dim).map(_ => Random.nextDouble()).toArray

  override protected def s(r: Vec): String = r.mkString("[", ",", "]")

  override protected def mutate(original: ItemAndFitness): ItemAndFitness = {
    val steplen = stepWidth(round)
    val dif = DoubleArrayVector.randomVector(dim) * steplen

    def dostep(x: ItemAndFitness) = new ItemAndFitness(x.item.zip(dif) map { case (x, d) => SimulatedAnnealing.mutateFunc(x, d) })

    var newitem = dostep(original)
    if (newitem.fitness > original.fitness) {
      var onestepmore = dostep(newitem)
      while (onestepmore.fitness > newitem.fitness) {
        newitem = onestepmore
        onestepmore = dostep(newitem)
      }
    }
    newitem
  }

  def stepping(): ItemAndFitness = {
    while (stepWidth(round) > 0) step()
    best()
  }
}

object SimulatedAnnealing {

  def stepWidthTiming(timeMin: Double, startWidth: Double = 0.1, endWidth: Double = 0.001): Int => Double = {
    val begin = System.currentTimeMillis()
    _ => {
      val ratio = (System.currentTimeMillis() - begin) / (TimeUnit.MINUTES.toMillis(1) * timeMin)
      if (ratio < 1) startWidth * Math.pow(endWidth / startWidth, ratio) else -1
    }
  }

  def stepWidthLog(numsteps: Int, startWidth: Double = 0.1, endWidth: Double = 0.001): Int => Double = stepNr => {
    if (stepNr < numsteps) startWidth * Math.pow(endWidth / startWidth, stepNr * 1.0 / numsteps) else -1
  }

  // def invf(x: Double) = Math.atan(x) * 2 / Math.PI
  // def f(x: Double) = Math.tan(x * Math.PI / 2)
  protected def mutateFunc(x: Double, dif: Double): Double = if (dif >= 0) Math.min(1.0, x + (1 - x) * dif) else Math.max(0, x * (1 + dif))
}

/** Simulates an evolution of a fixed population. */
abstract class Evolution[REP](populationsize: Int, fitnessFunction: REP => Double,
                              reductionProbability: Double = 0.01, keepEvenIfUpwardsProbability: Double = 0.1) {

  case class ItemAndFitness(item: REP, fitness: Double) {
    def this(item: REP) = this(item, fitnessFunction(item))

    override def toString: String = fitness + ":" + s(item)
  }

  var population: Array[ItemAndFitness] = 0.until(populationsize).map(_ => makeRandomItem()).map(new ItemAndFitness(_)).toArray
  var round: Int = 0

  protected def makeRandomItem(): REP

  protected def mutate(original: ItemAndFitness): ItemAndFitness

  protected def s(r: REP): String = r.toString

  protected def mutateOrKeep(original: ItemAndFitness): ItemAndFitness = {
    val newItem = mutate(original)
    if (newItem.fitness >= original.fitness) newItem
    else if (keepEvenIfUpwardsProbability > Random.nextDouble()) original
    else original
  }

  def step(): Unit = {
    round += 1
    if (reductionProbability > Random.nextDouble()) {
      population = population.sortBy(_.fitness)
      population = population.takeRight(populationsize / 2) ++ population.takeRight(populationsize - populationsize / 2)
    }
    population = population.map(mutateOrKeep)
  }

  def best(): ItemAndFitness = population.maxBy(_.fitness)

}
