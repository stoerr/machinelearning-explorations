package net.stoerr

import java.io.File
import java.text.{DateFormat, NumberFormat}
import java.util.{Date, Locale}

import scala.collection.immutable.{TreeMap, SortedMap}
import scala.io.Source

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 27.10.2014
 */
object OnVistaPriceParser {

  // Datum	Eröffnung	Tief	Hoch	Schluss
  val priceRegex = """<tr align="right"><td>([0-9][0-9]\.[0-9][0-9]\.[0-9][0-9])&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td><td>([0-9\.]+(?:,[0-9]+))&nbsp;</td></tr>""".r

  val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, Locale.GERMANY)
  val numberFormat = NumberFormat.getNumberInstance(Locale.GERMANY)

  def readFile(file: String) : SortedMap[Date, Double] = {
    val stream = OnVistaPriceParser.getClass.getClassLoader.getResourceAsStream(file)
    val entries = Source.fromInputStream(stream, "windows-1252").getLines().flatMap(priceRegex.findAllMatchIn(_)).map { m =>
      dateFormat.parse(m.group(1)) -> (numberFormat.parse(m.group(2)).doubleValue() + numberFormat.parse(m.group(3)).doubleValue()
        + numberFormat.parse(m.group(4)).doubleValue() + numberFormat.parse(m.group(5)).doubleValue()) / 4
    }
    SortedMap[Date, Double]() ++ entries.toMap
  }

  // Indizes 19.10.04 - 24.10.14
  // Options: ca. 28.07.11 - 24.10.14
  val dax = readFile("OnVista/Dax10Jahre.htm")
  val daxCall5000 = readFile("OnVista/DaxCall5000.htm")
  val daxCall11000 = readFile("OnVista/DaxCall11000.htm")
  val daxPut5000 = readFile("OnVista/DaxPut5000.htm")
  val daxPut11000 = readFile("OnVista/DaxPut11000.htm")
  val dowJones = readFile("OnVista/DowJones10J.htm")
  val sp500 = readFile("OnVista/SP500-10J.htm")

  val endDate = dateFormat.parse("24.10.14")
  val startDate = dateFormat.parse("28.7.11")

  def main(args: Array[String]): Unit = {
    dax.foreach { m =>
      println(m)
    }
  }

}
