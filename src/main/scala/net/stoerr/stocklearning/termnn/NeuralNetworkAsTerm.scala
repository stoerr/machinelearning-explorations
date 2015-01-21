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

  private def makeWeightVariable(i: Int, o: Int): Variable = Variable(layerName + ":" + i + ":" + o)

  val weightVariableMap: Map[(Int, Int), Variable] = (0 until inputCount).flatMap { i =>
    (0 until outputCount).map(o => ((i, o), makeWeightVariable(i, o)))
  }.toMap

  val weightVariables: IndexedSeq[Variable] = weightVariableMap.toList.sortBy(_._1).map(_._2).toIndexedSeq

  def termFunction(inputs: IndexedSeq[Term]): Term =
    (0 until inputCount).flatMap { i =>
      (0 until outputCount).map(o => weightVariableMap((i, o)) * inputs(i))
    }.reduce(_ + _)

}
