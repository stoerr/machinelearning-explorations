package net.stoerr.stocklearning

import net.stoerr.stocklearning.data.CryptoData

import scala.reflect.io.Path

object SummarizeCryptoData extends App {

  private val path = Path(SummarizeCryptoData.getClass.getResource("/CryptoDataDownload/0pairs.txt").getFile).parent

  val files = path.jfile.listFiles().filter(f => f.getName.endsWith(".csv"))

  val datasets = files.map(f => CryptoData(f.getPath.replaceFirst(".*/CryptoDataDownload", "CryptoDataDownload"))).sortBy(-_.data.length).sortBy(_.data(0).symbol.name)

  datasets.foreach(d => {
    println(d.path)
    println(s"${d.data.length} days of ${d.currencypair}" )
    println(s"${d.data.map(_.date).min} to ${d.data.map(_.date).max}")
    println(s"price from ${d.data.map(_.med).min} to ${d.data.map(_.med).max}")
    println(s"Sum volume from ${d.data.map(_.volume).sum} , volume to ${d.data.map(_.volume).sum}")
    println()
  })

  datasets.foreach(d => println(s"""val ${d.currencypair.toLowerCase()} = CryptoData("${d.path}")"""))

  println(datasets.map(_.currencypair.toLowerCase).mkString(","))

}
