package net.stoerr.stocklearning.termnn

import net.stoerr.stocklearning.calculationcompiler._

import scala.collection.immutable.IndexedSeq

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 20.01.2015
 */
object NeuralNetworkAsTerm {


}

case class SimpleNNLayer(inputCount: Int, outputCount: Int, layerName: String) {

  private val weightVariableMap: Map[(Int, Int), Variable] = Term.variableMatrix(layerName, inputCount, outputCount)

  val weightVariables: IndexedSeq[Variable] = weightVariableMap.toList.sortBy(_._1).map(_._2).toIndexedSeq

  def termFunction(inputs: IndexedSeq[Term]) =
    (0 until outputCount).map { o => (0 until inputCount).map(i => weightVariableMap(i, o) * inputs(i)).reduce(_ + _)}

}
