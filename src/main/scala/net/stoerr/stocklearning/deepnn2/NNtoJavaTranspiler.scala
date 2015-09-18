package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.calculationcompiler.Term

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.09.2015
 */
class NNtoJavaTranspiler {

  def orderCalculation(terms: Traversable[NNTerm]) : Vector[Vector[NNTerm]] = {
    var stages : List[Vector[NNTerm]] = List(terms.toSet.toVector.sorted)
    while(!stages.head.isEmpty) {
      var nextStage = stages.head.flatMap(_.subterms).toVector.sorted
      stages = nextStage :: stages
      var cleanupTerms = nextStage.flatMap(_.subterms).toSet
      stages = stages.map(s => s.filterNot(t => cleanupTerms.contains(t)))
    }
    stages.tail.toVector
  }

}
