package net.stoerr.stocklearning

import java.net.Proxy.Type
import java.net.{InetSocketAddress, Proxy, URL}

import scala.io.Source

/**
 * Grabs stock prices from OnVista, e.g. for dax: http://www.onvista.de/index/quote_history.html?ID_NOTATION=20735&RANGE=24M .
 * @deprecated Does not work yet.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 27.10.2014
 */
@deprecated
object OnVistaPriceGrabber {

  // Datum	Eröffnung	Tief	Hoch	Schluss
  val pricePattern = """<tr class="hr"><td class="hc">([0-9][0-9]\.[0-9][0-9]\.[0-9][0-9])&nbsp;</td><td>([0-9]+(?:,[0-9]+)?)&nbsp;</td><td>([0-9]+(?:,[0-9]+)?)&nbsp;</td><td>([0-9]+(?:,[0-9]+)?)&nbsp;</td><td>([0-9]+(?:,[0-9]+)?)&nbsp;</td></tr>""".r

  def grab(url: String) = {
    val proxy = new Proxy(Type.HTTP, new InetSocketAddress("proxy", 8080))
    val conn = new URL(url).openConnection(proxy);
    val src = Source.fromInputStream(conn.getInputStream, "windows-1252");
    val matches = src.getLines().flatMap(pricePattern.findAllMatchIn(_))
    val quotes = matches.map{ m =>
      m.group(1) -> m.group(2)
    }
    quotes.toMap
  }

  def main(args: Array[String]) {
    println(grab("http://www.onvista.de/index/quote_history.html?ID_NOTATION=20735&RANGE=24M"));
    println("Done")
  }

}
