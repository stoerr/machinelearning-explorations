package net.stoerr.stocklearning.unified

import net.stoerr.stocklearning.common.DoubleArrayVector._
import net.stoerr.stocklearning.common.RandomPseudoGradientDescent
import net.stoerr.stocklearning.deepnn.DeepNN
import net.stoerr.stocklearning.genetic.cgp.{CGPEvolution, CGPGene}
import net.stoerr.stocklearning.unified.FitnessFunctions.FitnessFunction
import net.stoerr.stocklearning.util.GitPrinter

object FitnessFunctions {

  type FitnessFunction = Function[Vec => Vec, Double]

  def sumAvgFunction(fitnesses: Seq[FitnessFunction]): FitnessFunction =
    func => fitnesses.map(_ (func)).toList.sum / fitnesses.size

  def multAvgFunction(fitnesses: Seq[FitnessFunction]): FitnessFunction =
    func => math.pow(fitnesses.map(_ (func)).toList.product, 1.0 / fitnesses.size)

  def minFunction(fitnesses: Seq[FitnessFunction]): FitnessFunction =
    func => fitnesses.map(fitness => fitness.apply(func)).toList.min

}

trait AbstractLearner[REP <: AnyRef] {

  def stepping(gene: REP, fitness: FitnessFunction, rounds: Range): REP

  def func(gene: REP): Vec => Vec

  var printIntermediate: REP => Unit = { _ => }

  def stepUntil(gene: REP, fitness: FitnessFunction, roundmax: Int): REP = {
    var result = gene
    for (rnd <- 0.until(roundmax, 200)) {
      result = stepping(result, fitness, rnd.until(rnd + 200))
      printIntermediate(result)
    }
    result
  }

  def competitiveStepping(gene: REP, fitness: FitnessFunction, roundmax: Int, numcomp: Int = 10,
                          prerounds: Int = 20000, competitionFitness: FitnessFunction = null): REP = {
    val compareFitness = if (competitionFitness != null) competitionFitness else fitness
    var best: REP = stepUntil(gene, fitness, prerounds)
    var bestFitness = compareFitness(func(best))
    for (pretry <- 1 until numcomp) {
      val trygene: REP = stepUntil(gene, fitness, prerounds)
      val trygenefitness = compareFitness(func(trygene))
      if (trygenefitness > bestFitness) {
        bestFitness = trygenefitness
        best = trygene
      }
    }
    println("=============== pretry finished ===============")
    stepUntil(best, fitness, roundmax)
  }
}

class CGPEvolutionLearner(insize: Int, outsize: Int, genecount: Int) extends AbstractLearner[CGPGene] {

  def stepping(init: CGPGene, fitness: FitnessFunction, rounds: Range): CGPGene = {
    val evolution = CGPEvolution(genecount, insize, outsize, fitness, init)
    for (rnd <- rounds) {
      val result: Double = evolution.step()
      if (rnd % 10 == 0) print(s"$rnd : $result\t")
    }
    println(s"\nlearned: ${evolution.best._1}")
    println(evolution.best._1.formula)
    evolution.best._1
  }

  def func(gene: CGPGene): Vec => Vec = gene.calculate

}

class DeepNNLearner(nn: DeepNN) extends AbstractLearner[Vec] {

  def stepping(weights: Vec, fitness: FitnessFunction, rounds: Range): Vec = {
    var result = weights

    if (result == null) result = randomVector(nn.sizeWeights) * 0.01

    def f(weights: Vec): Double = -fitness(func(weights))

    val stepper = RandomPseudoGradientDescent(f, nn.sizeWeights, result)
    for (rnd <- rounds) {
      val stepresult = stepper.stepOrthogonalRandom()
      print(s"$rnd : $stepresult\t")
    }
    GitPrinter.printGitinfo()
    println("\nlearned weights: Array(" + stepper.x.mkString(", ") + ")")
    stepper.x
  }

  def func(weights: Vec): Vec => Vec = nn.f(_)(weights)

}
