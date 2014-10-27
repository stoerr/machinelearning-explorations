package net.stoerr.stocklearning

import java.text.{DateFormat, NumberFormat}
import java.util.{Date, Locale}

import scala.collection.immutable.SortedMap
import scala.io.Source

/**
 * Some stock quote histories as learning examples
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 27.10.2014
 */
object StockQuoteRepository {

  // Datum	Eröffnung	Tief	Hoch	Schluss
  val priceRegex = """<tr align="right"><td>([0-9][0-9]\.[0-9][0-9]\.[0-9][0-9])&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td></tr>""".r

  val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, Locale.GERMANY)
  val numberFormat = NumberFormat.getNumberInstance(Locale.GERMANY)

  def readOnVistaFile(file: String) : SortedMap[Date, Double] = {
    val stream = StockQuoteRepository.getClass.getClassLoader.getResourceAsStream(file)
    val entries = Source.fromInputStream(stream, "windows-1252").getLines().flatMap(priceRegex.findAllMatchIn(_)).map { m =>
      dateFormat.parse(m.group(1)) -> (numberFormat.parse(m.group(2)).doubleValue() + numberFormat.parse(m.group(3)).doubleValue()
        + numberFormat.parse(m.group(4)).doubleValue() + numberFormat.parse(m.group(5)).doubleValue()) / 4
    }
    SortedMap[Date, Double]() ++ entries.toMap
  }

  // Indizes 19.10.04 - 24.10.14
  // Options: ca. 28.07.11 - 24.10.14
  lazy val dax = readOnVistaFile("OnVista/Dax10Jahre.htm")
  lazy val daxCall5000 = readOnVistaFile("OnVista/DaxCall5000.htm")
  lazy val daxCall11000 = readOnVistaFile("OnVista/DaxCall11000.htm")
  lazy val daxPut5000 = readOnVistaFile("OnVista/DaxPut5000.htm")
  lazy val daxPut11000 = readOnVistaFile("OnVista/DaxPut11000.htm")
  lazy val dowJones = readOnVistaFile("OnVista/DowJones10J.htm")
  lazy val sp500 = readOnVistaFile("OnVista/SP500-10J.htm")

  val endDate = dateFormat.parse("24.10.14")
  /** start date for the options - the indices go farther back. */
  val startDate = dateFormat.parse("28.7.11")

  def main(args: Array[String]): Unit = {
    dax.foreach { m =>
      println(m)
    }
  }

}
