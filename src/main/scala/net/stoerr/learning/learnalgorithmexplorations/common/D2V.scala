package net.stoerr.learning.learnalgorithmexplorations.common

/**
 * Double value that keeps the first and second derivation after some variable.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 12.11.2014
 */
case class D2V(v: Double, d: Double = 0, d2: Double = 0) {

  def +(o: D2V): D2V = D2V(v + o.v, d + o.d, d2 + o.d2)

  def -(o: D2V): D2V = D2V(v - o.v, d - o.d, d2 - o.d2)

  def *(o: D2V): D2V = D2V(v * o.v, v * o.d + d * o.v, 2 * d * o.d + d2 * o.v + o.d2 * v)

  def /(o: D2V): D2V = D2V(v / o.v, (d * o.v - v * o.d) / (o.v * o.v),
    (2 * (v * o.d * o.d - d * o.v * o.d) - v * o.v * o.d2 + o.v * o.v * d2)
      / (o.v * o.v * o.v)
  )

  def abs: D2V = if (v < 0) D2V(-v, -d, -d2) else this

  def log: D2V = D2V(math.log(v), d / v, d2 / v - d * d / (v * v))

}
