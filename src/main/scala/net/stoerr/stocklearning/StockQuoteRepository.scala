package net.stoerr.stocklearning

import java.text.{DateFormat, NumberFormat}
import java.util.concurrent.TimeUnit
import java.util.{Date, Locale}

import scala.io.Source
import scala.collection.{mutable, immutable}

/**
 * Some stock quote histories as learning examples
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 27.10.2014
 */
object StockQuoteRepository {

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
  def readOnVistaFile(file: String): immutable.SortedMap[Int, Double] = {
    val stream = StockQuoteRepository.getClass.getClassLoader.getResourceAsStream(file)
    val entries = Source.fromInputStream(stream, "windows-1252").getLines()
      .flatMap(priceRegex.findAllMatchIn(_)).map {
      m =>
        val key = weekdayNumber(dateFormat.parse(m.group(1))) - weekdayNumber(endDate)
        key ->
          (numberFormat.parse(m.group(2)).doubleValue() + numberFormat.parse(m.group(3)).doubleValue()
            + numberFormat.parse(m.group(4)).doubleValue() + numberFormat.parse(m.group(5)).doubleValue()) / 4
    }.toList
    assert(!entries.isEmpty, file)
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
  val dax = readOnVistaFile("OnVista/Dax10Jahre.htm")
  val daxCall5000 = readOnVistaFile("OnVista/DaxCall5000.htm")
  val daxCall11000 = readOnVistaFile("OnVista/DaxCall11000.htm")
  val daxPut5000 = readOnVistaFile("OnVista/DaxPut5000.htm")
  val daxPut11000 = readOnVistaFile("OnVista/DaxPut11000.htm")
  val dowJones = readOnVistaFile("OnVista/DowJones10J.htm")
  val sp500 = readOnVistaFile("OnVista/SP500-10J.htm")

  val options = Array(daxCall5000, daxPut5000, daxCall11000, daxPut11000)
  val onames = Array("c5", "p5", "c11", "p11")

  def main(args: Array[String]): Unit = {
    dax.foreach {
      m =>
        println(m)
    }
  }

}
