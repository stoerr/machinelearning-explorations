package net.stoerr.stocklearning

import org.scalatest.FunSuite

/**
 * Tests for {@link DValue}
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.10.2014
 */
class TestDValue extends FunSuite {

  test("just print something") {
    println(DValue(1.5))
    println(DValue(2.7, "v"))
    val v1 = DValue(1.1, "v1")
    val v2 = DValue(2.3, "v2")
    println(v1 + v2)
    println(v1 + v2 + v2)
    println(v1 + v2 + v1 + v2)
  }

}
