package net.stoerr.learning.learnalgorithmexplorations.deepnn2

import org.scalatest.funsuite.AnyFunSuite

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 29.08.2015
 */
class TestNNtoJavaTranspiler extends AnyFunSuite {
  val I0 = I("0")
  val I1 = I("1")
  val O0 = O("0")
  val c1 = (I0 + I1) * O0 + (I0 + I1)
  val c2 = I0 + O0 * I1

  test("orderCalculation") {
    assert(Vector(Vector(I0)) == NNtoJavaTranspiler.orderCalculation(List(I0)))
    val orderedc1 = NNtoJavaTranspiler.orderCalculation(List(c1))
    // println(orderedc1)
    assert(Vector(Vector(I0, I1), Vector(I0 + I1, O0), Vector((I0 + I1) * O0), Vector(c1)) == orderedc1)
    val ordered2 = NNtoJavaTranspiler.orderCalculation(List(c1, c2))
    // println(ordered2)
    // ordered2 foreach println
    assert(Vector(Vector(I0, I1), Vector((I0 + I1), O0), Vector((I0 + I1) * O0, I1 * O0), Vector(((I0 + I1) * O0 + I0 + I1), (I0 + I1 * O0))) == ordered2)

  }

  test("Transpiler") {
    new NNtoJavaTranspiler(Set(c1, c2))
    println()
    val nn = NNCreator.simpleNetwork(List(2, 5, 5, 1))
    val evalterm = nn.evaluationTerm
    val derivs = evalterm.wDerivative
    new NNtoJavaTranspiler(evalterm.toplevelNnSubterms.toSet)
    println()
    val toplevelterms = (Set(evalterm) ++ derivs.values).flatMap(_.toplevelNnSubterms.toSet)
    new NNtoJavaTranspiler(toplevelterms)
  }

}
