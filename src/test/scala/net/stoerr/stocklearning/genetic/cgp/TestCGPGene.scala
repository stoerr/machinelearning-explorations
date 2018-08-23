package net.stoerr.stocklearning.genetic.cgp

import net.stoerr.stocklearning.data.{BuySimulator, CryptoData}
import org.scalatest.FunSuite

class TestCGPGene extends FunSuite {

  test("Function calculation") {
    val gene = CGPGene(
      Array(0.001, 0.002, 0.503, 0.7, 0.8), 2, 1
    )
    val res = gene.calculate(Array(0.1, 0.2))
    assert(res.length == 1)
    assert(Math.abs(res(0) - 0.15) < 0.00001)
    println(gene.formula())
    assert("o0 = c0\nc0 = Add(in0, in1, 0.7)\n" == gene.formula())
  }

  test("Approximate xor") {
    val examples: Seq[(Array[Double], Double)] = Seq(Array(0.0, 0.0) -> 0.0, Array(0.0, 1.0) -> 1.0, Array(1.0, 0.0) -> 1.0, Array(1.0, 1.0) -> 0.0)
      .map(p => (p._1, p._2))
    val evolution = CGPEvolution(5, 2, 1, CGPEvolution.approximationFitness(examples))
    println(evolution.best._2)
    0 until 1000 foreach (_ => evolution.step())
    println(evolution.best._2)
    val approxfunc: Array[Double] => Array[Double] = evolution.best._1.calculate _
    examples foreach (e => println(e._1.toList + " : " + e._2 + " vs. " + approxfunc(e._1).toList))
    println(evolution.best._1.formula())
  }

  test("read file") {
    println(
      CryptoData.btcusd.data.take(10).foreach(println)
    )
  }

  test("buysimulator") {
    assertResult((200, 0))(BuySimulator.step(100, 200, 2, 1))
    assertResult((100, 200))(BuySimulator.step(100, 200, 2, 0.5))
    assertResult((0, 400))(BuySimulator.step(100, 200, 2, 0))

    assertResult((199, 0))(BuySimulator.stepWithFee(100, 200, 2, 1))
    assertResult((100, 200))(BuySimulator.step(100, 200, 2, 0.5))
    assertResult((0, 398))(BuySimulator.stepWithFee(100, 200, 2, 0))
  }

}
