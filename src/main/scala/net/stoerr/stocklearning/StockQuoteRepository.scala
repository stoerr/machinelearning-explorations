package net.stoerr.stocklearning

import java.text.{DateFormat, NumberFormat}
import java.util.concurrent.TimeUnit
import java.util.{Date, Locale}

import scala.collection.immutable.SortedMap
import scala.collection.{immutable, mutable}
import scala.io.Source

/**
 * Some stock quote histories as learning examples
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 27.10.2014
 */
object StockQuoteRepository {

  type StockData = immutable.SortedMap[Int, Double]

  def rtag(name: String, content: String) = "(?i)<" + name + """[^>]*>\s*+""" + content + """(?:&nbsp;)*\s*+</\s*+""" + name + """\s*+>\s*+"""

  val rnum = """((?:[0-9]+\.)*[0-9]+(?:,[0-9]+(?:,[0-9]+)*))"""
  val rdate = """([0-9]?[0-9]\.[0-9]?[0-9]\.(?:19|20)?[0-9][0-9])"""

  // Datum	Eröffnung	Tief	Hoch	Schluss
  val priceRegex = rtag("tr", rtag("td", rdate) + rtag("td", rnum) + rtag("td", rnum) + rtag("td", rnum) + rtag("td", rnum)).r

  val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, Locale.GERMANY)
  val numberFormat = NumberFormat.getNumberInstance(Locale.GERMANY)

  val endDate = dateFormat.parse("24.10.14")
  /** start date for the options - the indices go farther back. */
  val startDate = dateFormat.parse("28.7.11")

  private val normalizationDate = dateFormat.parse("18.10.04") // a monday

  /** number of weekdays after normalizationDate */
  def weekdayNumber(date: Date): Int = {
    val daycount = TimeUnit.DAYS.convert(date.getTime - normalizationDate.getTime, TimeUnit.MILLISECONDS)
    assert(daycount % 7 < 5, daycount + " - " + date + " : " + daycount % 7)
    (daycount % 7 + 5 * (daycount / 7)).asInstanceOf[Int]
  }

  /** Reads files saved like http://www.onvista.de/index/quote_history.html?ID_NOTATION=20735&RANGE=120M */
  def readOnVistaFile(file: String): StockData = {
    val stream = StockQuoteRepository.getClass.getClassLoader.getResourceAsStream(file)
    val entries = Source.fromInputStream(stream, "windows-1252").getLines()
      .flatMap(priceRegex.findAllMatchIn(_)).map {
      m =>
        val key = weekdayNumber(dateFormat.parse(m.group(1))) - weekdayNumber(endDate)
        key ->
          (numberFormat.parse(m.group(2)).doubleValue() + numberFormat.parse(m.group(3)).doubleValue()
            + numberFormat.parse(m.group(4)).doubleValue() + numberFormat.parse(m.group(5)).doubleValue()) / 4
    }.toList
    assert(entries.nonEmpty, file)
    assert(entries.map(_._1).toSeq.groupBy(x => x).filter(_._2.size > 1).isEmpty)
    interpolate(immutable.SortedMap[Int, Double]() ++ entries.toMap)
  }

  def interpolate(map: immutable.SortedMap[Int, Double]): immutable.SortedMap[Int, Double] = {
    var first = true
    var lastkey: Int = 0
    var lastvalue: Double = 0
    val interpolates = mutable.Buffer[(Int, Double)]()
    map.map {
      case (key, value) =>
        if (!first && key > lastkey + 1) {
          (lastkey + 1).until(key).foreach {
            ikey: Int =>
              interpolates += ((ikey, lastvalue + (value - lastvalue) * (ikey - lastkey) / (key - lastkey)))
          }
        }
        lastkey = key
        lastvalue = value
        first = false
    }
    map ++ interpolates
  }

  // Indizes 19.10.04 - 24.10.14
  // Options: ca. 28.07.11 - 24.10.14
  /** 3.1422824099766394E-4 +- 0.011006386716428129 [ -0.09471475078445749 , 0.07082838671829854 ] : 2613 */
  val dax: StockData = readOnVistaFile("OnVista/Dax10Jahre.htm")
  /** 2.303681448495171E-4 +- 0.02429727213749672 [ -0.14016940101838293 , 0.12239977552894699 ] : 848 */
  val daxCall5000: StockData = readOnVistaFile("OnVista/DaxCall5000.htm")
  /** -0.0017788975653484837 +- 0.05082902522246617 [ -0.2237230934921227 , 0.20139480845541347 ] : 846 */
  val daxCall11000: StockData = readOnVistaFile("OnVista/DaxCall11000.htm")
  /** -0.0033625339433343802 +- 0.04660592152180133 [ -0.19782574332991978 , 0.24659521234925932 ] : 848 */
  val daxPut5000: StockData = readOnVistaFile("OnVista/DaxPut5000.htm")
  /** -7.414818557628482E-4 +- 0.024712814029127107 [ -0.10024261013472781 , 0.09831428225436685 ] : 846 */
  val daxPut11000: StockData = readOnVistaFile("OnVista/DaxPut11000.htm")
  /** 1.992750056243933E-4 +- 0.007856292270844598 [ -0.05668684733102446 , 0.06285033570920469 ] : 2613 */
  val dowJones: StockData = readOnVistaFile("OnVista/DowJones10J.htm")
  /** 2.1706690482904415E-4 +- 0.008663687702549536 [ -0.06493104182489337 , 0.0696554632171355 ] : 2613 */
  val sp500: StockData = readOnVistaFile("OnVista/SP500-10J.htm")

  val options = Array(daxCall5000, daxPut5000, daxCall11000, daxPut11000)
  val onames = Array("c5", "p5", "c11", "p11")

  val maxIndex = daxCall5000.keys.reduce(math.max) - 1
  val minIndex = daxCall5000.keys.reduce(math.min)

  def main(args: Array[String]): Unit = {
    stockStats("dowJones", dowJones)
    stockStats("sp500", sp500)
    stockStats("dax", dax)
    stockStats("daxCall5000", daxCall5000)
    stockStats("daxCall11000", daxCall11000)
    stockStats("daxPut5000", daxPut5000)
    stockStats("daxPut11000", daxPut11000)
  }

  def stockStats(name: String, stock: SortedMap[Int, Double]): Unit = {
    val statistics = new Statistics("log change of " + name)
    stock.toSeq.sliding(2).map(s => math.log(s(1)._2 / s(0)._2)).foreach(statistics += _)
    println(statistics)
  }

}
