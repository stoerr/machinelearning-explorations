package net.stoerr.stocklearning.genetic

import scala.util.Random

/**
 * Genetic altgorithm: selection
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.01.2015
 */
case class Selection[COMPETITOR](domain: SelectionDomain[COMPETITOR], populationSize: Int,
                                 freshRatio: Double, mutationRatio: Double, crossoverRatio: Double) {

  case class Competitor(c: COMPETITOR, fitness: Double) {
    def this(c: COMPETITOR) = this(c, domain.fitness(c))
  }

  var population: Vector[Competitor] = Vector.fill(populationSize)(new Competitor(domain.make))

  def best = population(0)

  private def part(ratio: Double): Int = (populationSize * ratio).toInt

  def step() = {
    val fresh = Vector.fill(part(freshRatio))(domain.make)
    val mutated = population.take(part(mutationRatio)).map(_.c).map(domain.mutate)
    // unclear: selection of xover individuals
    val xover = Vector.fill(part(crossoverRatio))(
      domain.crossover(population(Random.nextInt(populationSize)).c, population(Random.nextInt(populationSize)).c)
    )
    val newpopulation = (fresh ++ mutated ++ xover).par.map(new Competitor(_))
    population = (population ++ newpopulation) sortBy (-_.fitness) take populationSize
  }

  private def selectFromProbabilityDistribution[T](probabilities: Traversable[(T, Double)]) = {
    require(probabilities.map(_._2).min >= 0)
    val sum = probabilities.map(_._2).sum
    // probabilities.scanLeft()
  }

}

trait SelectionDomain[COMPETITOR] {
  def make: COMPETITOR

  def fitness(c: COMPETITOR): Double

  def mutate(c: COMPETITOR): COMPETITOR

  def crossover(c1: COMPETITOR, c2: COMPETITOR): COMPETITOR
}
