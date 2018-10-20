package net.stoerr.stocklearning.genetic.cgp

import net.stoerr.stocklearning.data.BuySimulator
import org.scalatest.FunSuite

import scala.util.Random

class TestCGPGene extends FunSuite {

  test("Function calculation") {
    val gene = CGPGene(2, 1, 1,
      Array(0.001, 0.002, 0.503, 0.7), Array(0.8))
    assert("CGPGene(2,1,1, Array(0.001,0.002,0.503,0.7), Array(0.8))" == gene.serializedFull)
    val res = gene.calculate(Array(0.1, 0.2))
    assert(res.length == 1)
    assert(Math.abs(res(0) - 0.12) < 0.00001)
    println(gene.formula)
    assert("o0 = c0\nc0 = Add(in0, in1, 0.7)\n" == gene.formula)
  }

  test("Approximate xor") {
    val examples: Seq[(Array[Double], Double)] = Seq(Array(0.0, 0.0) -> 0.0, Array(0.0, 1.0) -> 1.0, Array(1.0, 0.0) -> 1.0, Array(1.0, 1.0) -> 0.0)
      .map(p => (p._1, p._2))
    val evolution = CGPEvolution(50, 2, 1, CGPEvolution.approximationFitness(examples))
    println(evolution.best._2)
    0 until 10000 foreach (_ => if (Math.abs(evolution.best._2) >= 0.05) evolution.step())
    println(evolution.best._2)
    val approxfunc: Array[Double] => Array[Double] = evolution.best._1.calculate _
    examples foreach (e => println(e._1.toList + " : " + e._2 + " vs. " + approxfunc(e._1).toList))
    println(evolution.best._1.formula)
    assert(Math.abs(evolution.best._2) < 0.05)
  }

  test("compactedSerialization") {
    val gene = CGPGene(3, 5, 2, Array(0.9443656349478684, 0.832366043365547, 0.09509106924259314, 0.2553637758024162, 0.050529724962731004, 0.5434401781891055, 0.5064991458658901, 0.09084597639930281, 0.9633954439531796, 0.09598770252539934, 0.43838837877257764, 0.9815971736978427, 0.3085578233341445, 0.9917033470529255, 0.30136596438700913, 0.15769134540151675, 0.5715174264686794, 0.7066732706343182, 0.5069829989220888, 0.40960817455152354), Array(0.43569885814383436, 0.44835431641883483))
    println(gene.serializedFull)
    println(gene.formula)
    println(gene.serialized)
    val gene2 = CGPGene(3, 5, 2, Array(0.9443656349478684, 0.832366043365547, 0.09509106924259314, 0.2553637758024162, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), Array(0.43569885814383436, 0.44835431641883483))
    println(gene2.serializedFull)
    println(gene2.formula)
    println(gene2.serialized)
    for (i <- 1 until 10) {
      val invec = 0.until(gene.numin).map(i => Random.nextDouble()).toArray
      0.until(gene.numout).foreach(i => assert(gene.calculate(invec)(i) == gene2.calculate(invec)(i)))
    }
  }

  test ("mutateUntilVisible") {
    val gene = new CGPGene(5, 50, 3)
    val invec = 0.until(gene.numin).map(i => Random.nextDouble()).toArray
    val result = gene.calculate(invec)
    var count = 0
    for (i <- 0 until 100) {
      val mutated = gene.mutateUntilVisible()
      val mresult = mutated.calculate(invec)
      var different = false
      for (o <- 0 until gene.numout)
        different = different || result(o) != mresult(o)
      if (different) count += 1
    }
    println(count)
    assert(count > 30)
  }

  test("buysimulator") {
    var buy = BuySimulator()
    assertResult((200, 0))(buy((100, 200), 2, 1000))
    assertResult((100, 200))(buy((100, 200), 2, 0))
    assertResult((0, 400))(buy((100, 200), 2, -1000))

    buy = BuySimulator(0.01)
    assertResult((199, 0))(buy((100, 200), 2, 1000))
    assertResult((100, 200))(buy((100, 200), 2, 0))
    assertResult((0, 398))(buy((100, 200), 2, -1000))
  }

}
