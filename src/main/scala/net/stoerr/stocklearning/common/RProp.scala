package net.stoerr.stocklearning.common

import net.stoerr.stocklearning.common.DoubleArrayVector._

/**
 * Resilient backpropagation: we only observe the signs of the gradient and change the step length
 * for each parameter
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 21.12.2014
 */
class RProp(val f: Array[Double] => Double, val fgrad: Array[Double] => (Double, Array[Double]),
            val maxSteps: Int, x0: Array[Double]) {

  private val etaminus = 0.5
  private val etaplus = 1.2

  var lval = Array.fill(x0.length)(1e-2)
  var lastsign = Array.fill(x0.length)(0.0)
  var x = x0

  def step() = {
    val (y, ygrad) = fgrad(x)
    println();
    println(y + "\t" + ygrad.toList)
    val gradsign = ygrad.signum
    println(lastsign.toList + "\t" + gradsign.toList)
    val lfac = gradsign * etaminus + (lastsign + gradsign) * ((etaplus - etaminus) / 2)
    println(lfac.toList)
    lval = lval elem_* lfac.elem_abs
    println(lval.toList)
    x = x - (gradsign elem_* lval)
    lastsign = gradsign
    println(x.toList)
  }

}
