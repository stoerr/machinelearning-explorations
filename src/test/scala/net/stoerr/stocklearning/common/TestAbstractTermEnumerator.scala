package net.stoerr.stocklearning.common

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.11.2014
 */
class TestAbstractTermEnumerator extends FunSuite {

  object SimpleTermEnumerator extends TermEnumerator[(Double, Double)] {

    type TwoVars = TermFunctionWithComplexity[(Double, Double), Double]

    val basic: Stream[TwoVars] = Stream(nullary("0.5", _ => 0.5), nullary("2", _ => 2),
      nullary("x", x => x._1), nullary("y", x => x._2))

    val terms: Stream[TwoVars] = basic #::: mergeStreams(
      terms map unary("-", -_),
      terms map unary("2 *", 2 * _),
      binaryOpStream[Double, Double, Double](terms, terms, "+", _ + _),
      binaryOpStream[Double, Double, Double](terms, terms, "*", _ * _),
      binaryOpStream[Double, Double, Double](terms, terms, "/", _ / _)
    )

  }

  test("Find terms for simple function") {
    val candidates = SimpleTermEnumerator.terms
      .filter(t => t.func(2, 2) == 6)
      .filter(t => t.func(1, 1) == 3)
      .filter(t => t.func(2, 1) == 5)

    candidates.take(10).foreach(println)
    assert(candidates.take(10).size == 10)
  }
}
