package net.stoerr.stocklearning.common

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.11.2014
 */
class TestAbstractTermEnumerator extends FunSuite {

  object SimpleTermEnumerator extends AbstractTermEnumerator {

    type TwoVars = TermFunctionWithComplexity[(Double, Double), Double]

    val basic: Stream[TwoVars] = Stream(nullary("0.5", _ => 0.5), nullary("2", _ => 2),
      nullary("x", x => x._1), nullary("y", x => x._2))

    val terms: Stream[TwoVars] = basic #::: mergeStreams(
      terms map unary("-", -_),
      terms map unary("2 *", 2 * _)
      // OUCH how to do binary correctly sorted?
    )

  }

  SimpleTermEnumerator.terms.take(20).foreach(println(_))

}
