package net.stoerr.stocklearning.genetic

import scala.util.Random

/**
 * Genetic altgorithm: selection
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.01.2015
 */
case class Selection[COMPETITOR](selectable: Selectable[COMPETITOR], populationSize: Int,
                                 freshRatio: Double, mutationRatio: Double, crossoverRatio: Double) {

  var population = Array.fill(populationSize)(selectable.make) sortBy (-selectable.fitness(_))

  def best = population(0)

  private def part(ratio: Double): Int = (populationSize * ratio).toInt

  def step() = {
    val fresh = Array.fill(part(freshRatio))(selectable.make)
    val mutated = population.take(part(mutationRatio)).map(selectable.mutate)
    // unclear: selection of xover individuals
    val xover = Array.fill(part(crossoverRatio))(
      selectable.crossover(population(Random.nextInt(populationSize)), population(Random.nextInt(populationSize)))
    )
    val newpopulation = fresh ++ mutated ++ xover
    population = (population.take(populationSize - newpopulation.size) ++ newpopulation) sortBy (-selectable.fitness(_))
  }

}

trait Selectable[COMPETITOR] {
  def make: COMPETITOR

  def fitness(c: COMPETITOR): Double

  def mutate(c: COMPETITOR): COMPETITOR

  def crossover(c1: COMPETITOR, c2: COMPETITOR): COMPETITOR
}
