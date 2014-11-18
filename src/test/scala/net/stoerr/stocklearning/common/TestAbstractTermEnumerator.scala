package net.stoerr.stocklearning.common

import org.scalatest.FunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.11.2014
 */
class TestAbstractTermEnumerator extends FunSuite {

  class SimpleTermEnumerator extends AbstractTermEnumerator {

    type TwoVars = TermFunctionWithComplexity[(Double, Double), Double]

    val constants: Stream[TwoVars] = Stream(constant("0.5", 0.5), constant("2", 2))

    // val terms: Stream[TwoVars] = mergeStreams()

  }

}
