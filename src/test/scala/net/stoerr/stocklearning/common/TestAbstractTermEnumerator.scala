package net.stoerr.stocklearning.common

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.11.2014
 */
class TestAbstractTermEnumerator extends FunSuite {

  object SimpleTermEnumerator extends TermEnumerator {

    type TwoVars = TermFunctionWithComplexity[(Double, Double), Double]

    val basic: Stream[TwoVars] = Stream(nullary("0.5", _ => 0.5), nullary("2", _ => 2),
      nullary("x", x => x._1), nullary("y", x => x._2))

    def r : (TwoVars, TwoVars) => TwoVars = binary("+", _ + _)

    val terms: Stream[TwoVars] = basic #::: mergeStreams(
      terms map unary("-", -_),
      terms map unary("2 *", 2 * _),
      binaryOpStream(r, terms, terms)
    )

  }

  SimpleTermEnumerator.terms.take(50).foreach(println(_))

}
