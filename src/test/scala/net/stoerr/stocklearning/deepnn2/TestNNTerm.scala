package net.stoerr.stocklearning.deepnn2

import NNTerm._
import SNNTerm._
import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */
class TestNNTerm extends FunSuite {

  println(W("1") + I("2") * 3.0)

  println(SUMMED(W("1")) + 2.5)

}
