package net.stoerr.learning.learnalgorithmexplorations.util

import plotly._
import Plotly._
import plotly.element._
import plotly.layout._

import scala.collection.mutable.ArrayBuffer

class DoublePlot(name: String, descr: String, first: String, firstType: AxisType, second: String, secondType : AxisType) {

  val buf = new ArrayBuffer[(Double, Double, Double)]()

  def add(x: Double, y: Double, z: Double): Unit = {
    buf += ((x, y, z))
  }

  def plot(): Unit = {
    Seq(
      Scatter(buf.map(_._1), buf.map(_._2), name = first, yaxis = AxisReference.Y1),
      Scatter(buf.map(_._1), buf.map(_._3), name = second, yaxis = AxisReference.Y2)
    ).plot(
      path = "target/plot.html", title = s"${name}, ${descr} ${GitPrinter.commitDescription}",
      yaxis1 = Axis(`type` = firstType, side = Side.Left, title = first),
      yaxis2 = Axis(`type` = secondType, side = Side.Right, title = second,
        overlaying = AxisAnchor.Y),
      height = 1000
    )
  }

}
