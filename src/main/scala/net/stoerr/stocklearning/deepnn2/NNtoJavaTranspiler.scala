package net.stoerr.stocklearning.deepnn2

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.09.2015
 */
object NNtoJavaTranspiler {

  def orderCalculation(terms: Traversable[NNTerm]): Vector[Vector[NNTerm]] = {
    var stages: List[Vector[NNTerm]] = List(terms.toSet.toVector.sorted)
    while (stages.head.nonEmpty) {
      val nextStage = stages.head.flatMap(_.subterms).distinct.sorted
      stages = nextStage :: stages
      val cleanupTerms = nextStage.flatMap(_.subterms).toSet
      stages = stages.map(s => s.filterNot(t => cleanupTerms.contains(t)))
    }
    stages.tail.toVector
  }

}

class NNtoJavaTranspiler(terms: Set[NNTerm]) {

  val ordered: Vector[Vector[NNTerm]] = {
    val variables: Set[NNTerm] = terms.flatMap(_.inputs).toSet[NNTerm] ++ terms.flatMap(_.outputs) ++ terms.flatMap(_.weights)
    NNtoJavaTranspiler.orderCalculation(terms).map(_.filterNot(variables.contains)).filterNot(_.isEmpty)
  }

  case class Calculation(terms: Vector[NNTerm], allocationBefore: Map[NNTerm, Int], allocationAfter: Map[NNTerm, Int]) {
    private val allocationAfterKeys: Set[NNTerm] = allocationAfter.keys.toSet
    require(terms.forall(allocationAfterKeys.contains))
    require(allocationBefore.values.size == allocationBefore.values.toSet.size, allocationBefore) // no duplicates
    require(allocationAfter.values.size == allocationAfter.values.toSet.size, allocationAfter) // no duplicates
  }

  var freeVariableIndices: Stream[Int] = Range(0, Int.MaxValue).toStream

  var currentAllocation: Map[NNTerm, Int] = Map()

  val calculations = for (restStages: Vector[Vector[NNTerm]] <- ordered.tails;
                          stage: Vector[NNTerm] <- restStages.headOption) yield {
    val subtermsNeededLater = restStages.flatMap(_.flatMap(_.subterms)).toSet
    val unusedAllocations = currentAllocation.filterKeys(term => !subtermsNeededLater.contains(term))
    val unusedAllocationKeys = unusedAllocations.values.toArray.sorted
    freeVariableIndices = unusedAllocationKeys ++: freeVariableIndices
    currentAllocation = currentAllocation -- unusedAllocations.keys

    val newVariableIndices = freeVariableIndices.take(stage.size).toVector
    freeVariableIndices = freeVariableIndices.drop(stage.size)

    val previousAllocation = currentAllocation
    currentAllocation = currentAllocation ++ stage.zip(newVariableIndices)
    Calculation(stage, previousAllocation, currentAllocation)
  }

  calculations foreach println

}