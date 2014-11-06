package net.stoerr.stocklearning

/**
 * Utility for timing stuff.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 06.11.2014
 */
class Timer(name: String = "timer") {

  private val begin: Long = System.currentTimeMillis()

  def time(): Float = 0.001f * (System.currentTimeMillis() - begin)

  override def toString() = name + ": " + time() + "s"

}
