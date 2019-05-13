package net.stoerr.learning.learnalgorithmexplorations.graph

import plotly._
import Plotly._
import plotly.element._
import plotly.layout._

object RunGraph extends App {

  val x = 0.0 to 10.0 by 0.1
  val y1 = x.map(d => 2.0 * d + util.Random.nextGaussian())
  val y2 = x.map(math.exp)

  val plot = Seq(
    Scatter(
      x, y1, name = "Approx twice",
      yaxis = AxisReference.Y1
    ),
    Scatter(
      x, y2, name = "Exp",
      yaxis = AxisReference.Y2
    )
  )

  plot.plot(
    path = "target/plot.html",
    title = "Curves",
    yaxis1 = Axis(`type` = AxisType.Linear, side = Side.Left, title = "Y1"),
    yaxis2 = Axis(`type` = AxisType.Log, side = Side.Right, title = "Y2", overlaying = AxisAnchor.Y)
  )

}
