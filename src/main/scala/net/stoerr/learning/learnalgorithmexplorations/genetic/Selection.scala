package net.stoerr.learning.learnalgorithmexplorations.genetic

import net.stoerr.learning.learnalgorithmexplorations.common.ProbabilityDistributionGenerator

import scala.collection.parallel.immutable.ParVector

/**
 * Genetic altgorithm: selection
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 25.01.2015
 */
case class Selection[COMPETITOR](domain: SelectionDomain[COMPETITOR], populationSize: Int,
                                 freshRatio: Double, mutationRatio: Double, crossoverRatio: Double = 0.0) {

  case class Competitor(c: COMPETITOR, fitness: Double) {
    def this(c: COMPETITOR) = this(c, domain.fitness(c))
  }

  var population: Vector[Competitor] = Vector.fill(populationSize)(new Competitor(domain.make))

  def best = population(0)

  private def part(ratio: Double): Int = (populationSize * ratio).toInt

  def step() = {
    val fresh = Vector.fill(part(freshRatio))(domain.make)
    val mutated = population.take(part(mutationRatio)).map(_.c).map(domain.mutate)

    val minfitness = population.map(_.fitness).min
    val xoverDistribution = new ProbabilityDistributionGenerator(population.map(p => (p.c,
      p.fitness - minfitness)))
    val xover = Vector.fill(part(crossoverRatio))(
      domain.crossover(xoverDistribution.draw(), xoverDistribution.draw())
    )
    val newpopulation: ParVector[Competitor] = (fresh ++ mutated ++ xover).par.map(new Competitor(_))
    population = (population ++ newpopulation) sortBy (-_.fitness) take populationSize
  }

}

trait SelectionDomain[COMPETITOR] {
  def make: COMPETITOR

  def fitness(c: COMPETITOR): Double

  def mutate(c: COMPETITOR): COMPETITOR

  def crossover(c1: COMPETITOR, c2: COMPETITOR): COMPETITOR = sys.error("Unimplemented.")
}


