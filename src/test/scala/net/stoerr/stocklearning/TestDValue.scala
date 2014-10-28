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
    println(v1 * v2.abs / v1.log)
  }

  val eps = 1e-7
  def deriv(f: Double => Double, x : Double) = (f(x+eps)-f(x-eps))/(2*eps)

  test("Try to estimate value") {
    def complicated(xr: Double, yr: Double) = {
      val x = DValue(xr, "x"); val y = DValue(yr, "y")
      (x * DValue(-3)).abs + y / (x.abs * y.log - y)
    }
    val x = 1.5
    val y = 2.7
    val res = math.abs(x * -3) + y / (math.abs(x) * math.log(y) - y)
    val dres: DValue = complicated(x, y)
    println(dres)
    assert(res == dres.value)
    assert(0 == dres.deriv("z"))
    assert(2 == dres.derivations.size)
    println(deriv(complicated(_,y).value, x))
    println(deriv(complicated(x,_).value, y))
    assert( (deriv(complicated(_,y).value, x) - dres.deriv("x")) < 100*eps)
    assert( (deriv(complicated(x,_).value, y) - dres.deriv("y")) < 100*eps)
  }

}
