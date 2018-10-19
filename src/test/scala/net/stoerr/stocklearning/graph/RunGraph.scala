package net.stoerr.stocklearning.graph

object RunGraph extends App {

 import plotly._
  import Plotly._

  val x = 0.0 to 10.0 by 0.1
  val y1 = x.map(d => 2.0 * d + util.Random.nextGaussian())
  val y2 = x.map(math.exp)

  val plot = Seq(
    Scatter(
      x, y1, name = "Approx twice"
    ),
    Scatter(
      x, y2, name = "Exp"
    )
  )

  plot.plot(
    title = "Curves"
  )

}
