package net.stoerr.learning.learnalgorithmexplorations.deepnn2

case class NNRepresentation(inputs: Vector[I], weights: Vector[W], outputs: Vector[O],
                            outputCalculations: Vector[NNTerm], evaluationTerm: SNNTerm) {
  override def toString = s"NNRepresentation(\n$inputs, \n$outputs,\n$outputCalculations,\n$evaluationTerm)"

  lazy val usedToplevelNNTerms: Set[NNTerm] = (
    evaluationTerm.toplevelNnSubterms ++ evaluationTerm.wDerivative.values.flatMap(_.toplevelNnSubterms)
    ).toSet
}

/**
 * Creates a NN with rectilinear activation function.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.09.2015
 */
object NNCreator {

  import NNTerm._

  def num(i: Int) = (1000 + i).toString.substring(2)

  def inputs(n: Int): Vector[NNTerm] = Range(0, n).map(i => I(num(i))).toVector

  def outputs(n: Int): Vector[NNTerm] = Range(0, n).map(i => O(num(i))).toVector

  def wireup(in: Seq[NNTerm], numOut: Int, layernum: Int): Vector[NNTerm] = {
    for (o <- 1 to numOut) yield {
      // val summands = for ((i, inum) <- in.zipWithIndex) yield i * W(num(layernum) + "-" + num(inum) + "-" + num(o))
      // Tanh(summands.reduce(_ + _))
      val summands = for ((i, inum) <- in.zipWithIndex) yield (i, W(num(layernum) + "-" + num(o) + "-" + num(inum)))
      SoftSign(sumProd(summands.toVector :+(C(1.0), W(num(layernum) + "-" + num(o)))))
    }
  }.toVector

  /** * Creates a NN with rectilinear activation function. */
  def simpleNetwork(layers: Seq[Int]): NNRepresentation = {
    val ins = inputs(layers.head)
    var terms = ins
    for ((width, layernum) <- layers.zipWithIndex.tail) terms = wireup(terms, width, layernum)
    val outs = outputs(layers.last)
    val calculatedToOutputDiff = (terms, outs).zipped.map(_ - _).map(t => Sqr(t).asInstanceOf[NNTerm]).reduce(_ + _)
    val evaluationFunction = SUMMED(calculatedToOutputDiff)
    val weights = calculatedToOutputDiff.componentStream.filter(_.isInstanceOf[W]).map(_.asInstanceOf[W]).toSet.toVector
    NNRepresentation(ins.asInstanceOf[Vector[I]], weights, outs.asInstanceOf[Vector[O]], terms, evaluationFunction)
  }

}

