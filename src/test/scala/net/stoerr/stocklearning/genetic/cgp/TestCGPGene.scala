package net.stoerr.stocklearning.genetic.cgp

import org.scalatest.FunSuite

import scala.util.Random

class TestCGPGene extends FunSuite {

  test("Function calculation") {
    val gene = CGPGene(2, 1, 1,
      Array(0.001, 0.002, 0.503, 0.7), Array(0.8))
    assert("CGPGene(2,1,1, Array(0.001,0.002,0.503,0.7), Array(0.8))" == gene.serializedFull)
    val res = gene.calculate(Array(0.1, 0.2))
    assert(res.length == 1)
    println(gene.formula)
    assert(Math.abs(res(0) - Add(0.1, 0.2, 0.7)) < 0.00001)
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

  test("mutateUntilVisible") {
    val samples = 100
    val rounds = 100
    var count = 0
    for (r <- 0 to rounds) {
      val gene = new CGPGene(5, 100, 3)
      val invec = 0.until(gene.numin).map(i => Random.nextDouble()).toArray
      val result = gene.calculate(invec)
      for (i <- 0 until samples) {
        val mutated = gene.mutateUntilVisible()
        val mresult = mutated.calculate(invec)
        var different = false
        for (o <- 0 until gene.numout)
          different = different || result(o) != mresult(o)
        if (different) count += 1
      }
    }
    println(count)
    assert(count > samples * rounds / 2)
  }

  test("dependsOn") {
    def depends(f: Double => Double): Boolean = f(Random.nextGaussian()) != f(Random.nextGaussian())

    val a1 = Stream.continually(Random.nextGaussian())
    val a2 = Stream.continually(Random.nextGaussian())
    for (func <- CGPFunction.values) {
      assert(func.dependsOn(1) == (0.to(20).exists(a => depends(func(_, a1(a), a2(a))))), s"$func - 1")
      assert(func.dependsOn(2) == (0.to(20).exists(a => depends(func(a1(a), _, a2(a))))), s"$func - 2")
      assert(func.dependsOn(3) == (0.to(20).exists(a => depends(func(a1(a), a2(a), _)))), s"$func - 3")
    }
  }

}
