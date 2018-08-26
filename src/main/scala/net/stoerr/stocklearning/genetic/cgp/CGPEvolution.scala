package net.stoerr.stocklearning.genetic.cgp

case class CGPEvolution(numcalc: Int, numin: Int, numout: Int, fitness: (Array[Double] => Array[Double]) => Double) {

  val keepBest = 1
  val createMutations = 4

  /** current population, ordered by fitness from best to least */
  var population: Seq[(CGPGene, Double)] = 0.until(keepBest + createMutations)
    .map(_ => new CGPGene(numcalc, numin, numout))
    .map(addfitness).sortBy(_._2)

  protected def addfitness(g: CGPGene): (CGPGene, Double) = (g, fitness(g.calculate))

  def step(): Unit = {
    val bests = population.take(keepBest)
    val fresh = bests.flatMap(gene =>
      0.until(createMutations).map(_ => gene._1.mutate()).map(addfitness)
    )
    population = (fresh ++ bests).sortBy(_._2)
  }

  def best: (CGPGene, Double) = population.head

}

object CGPEvolution {
  def approximationFitness(examples: Seq[(Array[Double], Double)]): (Array[Double] => Array[Double]) => Double = { func =>
    def sqr(x: Double) = x * x

    Math.sqrt(examples.par.map(e => sqr(func(e._1)(0) - e._2)).sum / examples.size)
  }

}
