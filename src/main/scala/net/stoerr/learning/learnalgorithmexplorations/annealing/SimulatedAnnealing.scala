package net.stoerr.learning.learnalgorithmexplorations.annealing

import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector
import net.stoerr.learning.learnalgorithmexplorations.common.DoubleArrayVector._

import scala.reflect.ClassTag
import scala.util.Random

/** Takes an optimization problem with a set of parameters (a [[Vec]] ) between 0 and 1 and tries to solve it using
  * random steps which decrease in size over time. The step size is adaptive: it adapts so that
  * the number of steps taken in each mutation is roughly 1 on average, and should thus decrease over time.
  * <p>
  * Jobs of the classes:
  * EvolutionWithFixedPopulation: keep a number of individuals, give them the chance to evolve and sometimes replace the worst with copies of the best.
  * Individual: Basic mutation functionality.
  * SimulatedAnnealing: Individual that works with simulated annealing as mutation.
  */
case class AdaptiveSimulatedAnnealing(dim: Int, fitnessFunction: Vec => Double, stepReduction: Double = 1.2,
                                      item: VecAndFitness = null,
                                      steplen: Double = 0.5, keepEvenIfUpwardsProbability: Double = 0.0,
                                      stepstaken: Int = 0, minsteplen: Double = 0.0001
                                     ) extends Individual[AdaptiveSimulatedAnnealing] {

  protected def mutateFunc(x: Double, dif: Double): Double = if (dif >= 0) Math.min(1.0, x + (1 - x) * dif) else Math.max(0, x * (1 + dif))

  protected def mkFItem(item: Vec): VecAndFitness = VecAndFitness(item, fitnessFunction(item))

  override def fitness: Double = if (item != null) item.fitness else Double.NegativeInfinity

  override def mutate(): AdaptiveSimulatedAnnealing = {
    var dif = DoubleArrayVector.randomVector(dim).normalize * steplen
    var round: Int = 0
    val roundgoal = 1

    def dostep(x: VecAndFitness) = mkFItem(x.item.zip(dif) map { case (x, d) => mutateFunc(x, d) })

    var newitem: VecAndFitness = null
    if (item == null) {
      newitem = mkFItem(0.until(dim).map(_ => Random.nextDouble()).toArray)
      round = 1
    } else {
      newitem = dostep(item)
      if (newitem.fitness > item.fitness) {
        round += 1
        var onestepmore = dostep(newitem)
        while (onestepmore.fitness > newitem.fitness) {
          newitem = onestepmore
          onestepmore = dostep(newitem)
          round += 1
        }
      }
    }
    // println(s"=== ${steplen}, $round")
    val outitem = if (item == null || newitem.fitness >= fitness) newitem
    else if (keepEvenIfUpwardsProbability > Random.nextDouble()) newitem
    else item
    val newsteplen = if (round < roundgoal) steplen / stepReduction else if (round == roundgoal) steplen else steplen * stepReduction
    this.copy(
      item = outitem,
      steplen = newsteplen,
      stepstaken = stepstaken + round + 1
    )
  }

  def findOptimum(maxsteps: Int = 1000): AdaptiveSimulatedAnnealing = {
    var run = this
    for (_ <- 0 until maxsteps if run.steplen > minsteplen) run = run.mutate()
    run
  }
}

case class VecAndFitness(item: Vec, fitness: Double) {
  override def toString: String = fitness + ":" + item.mkString("[", ",", "]")
}

/** Simulates an evolution of a fixed population. */
abstract class EvolutionWithFixedPopulation[REP <: Individual[REP] : ClassTag]
(populationsize: Int, reductionProbability: Double = 0.01) {

  var population: Array[REP] = 0.until(populationsize).map(_ => makeRandomItem()).toArray
  var round: Int = 0

  protected def makeRandomItem(): REP

  def step(): Unit = {
    round += 1
    if (reductionProbability > Random.nextDouble()) {
      population = population.sortBy(_.fitness)
      population = population.takeRight(populationsize / 2) ++ population.takeRight(populationsize - populationsize / 2)
    }
    population = population.map(_.mutate())
  }

  def best(): REP = population.maxBy(_.fitness)

}

trait Individual[I <: Individual[I]] {
  def mutate(): I

  def fitness: Double
}

trait IndividualWithCrossover[I <: IndividualWithCrossover[I]] extends Individual[I] {
  def crossover(other: this.type): this.type
}

case class SimulatedAnnealingEvolution(dim: Int, fitnessFunction: Vec => Double, populationsize: Int = 10, reductionProbability: Double = 0.01, minsteplen: Double = 0.0001)
  extends EvolutionWithFixedPopulation[AdaptiveSimulatedAnnealing](populationsize, reductionProbability) {

  override protected def makeRandomItem(): AdaptiveSimulatedAnnealing =
    new AdaptiveSimulatedAnnealing(dim, fitnessFunction, minsteplen = minsteplen).mutate()

  def findOptimum(maxsteps: Int = 1000): AdaptiveSimulatedAnnealing = {
    for (_ <- 0 until maxsteps if best.steplen > minsteplen) step()
    best()
  }

}
