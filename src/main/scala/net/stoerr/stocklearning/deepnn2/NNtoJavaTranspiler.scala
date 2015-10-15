package net.stoerr.stocklearning.deepnn2

import net.stoerr.stocklearning.java.deepnn2.{AbstractNNJavaEvaluator, JavaCompilerWithResourceLoad}

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.09.2015
 */
object NNtoJavaTranspiler {

  def orderCalculation(terms: Traversable[NNTerm]): Vector[Vector[NNTerm]] = {
    var stages: List[Vector[NNTerm]] = List(terms.toSet.toVector.sorted)
    while (stages.head.nonEmpty) {
      val nextStage = stages.head.flatMap(_.subterms).distinct.sorted
      stages = nextStage :: stages
      val cleanupTerms = nextStage.flatMap(_.subterms).toSet
      stages = stages.map(s => s.filterNot(t => cleanupTerms.contains(t)))
    }
    stages.tail.toVector
  }

}

class NNtoJavaTranspiler(terms: Set[NNTerm]) {

  private val begintime = System.currentTimeMillis()

  val ordered: Vector[Vector[NNTerm]] = {
    val variables: Set[NNTerm] = terms.flatMap(_.inputs).toSet[NNTerm] ++ terms.flatMap(_.outputs) ++ terms.flatMap(_
      .weights)
    NNtoJavaTranspiler.orderCalculation(terms).map(_.filterNot(variables.contains)).filterNot(_.isEmpty)
  }

  case class Calculation(terms: Vector[NNTerm], allocationBefore: Map[NNTerm, Int], allocationAfter: Map[NNTerm, Int]) {
    private val allocationAfterKeys: Set[NNTerm] = allocationAfter.keys.toSet
    require(terms.forall(t => allocationAfterKeys.contains(t) || terms.contains(t)))
    require(allocationBefore.values.size == allocationBefore.values.toSet.size, allocationBefore) // no duplicates
    require(allocationAfter.values.size == allocationAfter.values.toSet.size, allocationAfter) // no duplicates
  }

  var freeVariableIndices: Stream[Int] = Range(0, Int.MaxValue).toStream

  var currentAllocation: Map[NNTerm, Int] = Map()

  val calculations: Seq[Calculation] =
    for (restStages: Vector[Vector[NNTerm]] <- ordered.tails.toSeq;
         stage: Vector[NNTerm] <- restStages.headOption) yield {
      val subtermsNeededLater = restStages.flatMap(_.flatMap(_.subterms)).toSet
      val unusedAllocations = currentAllocation.filterKeys(term => !subtermsNeededLater.contains(term))
      val unusedAllocationKeys = unusedAllocations.values.toArray.sorted
      freeVariableIndices = unusedAllocationKeys ++: freeVariableIndices
      currentAllocation = currentAllocation -- unusedAllocations.keys

      val stageWithoutResults = stage.filterNot(terms)
      val newVariableIndices = freeVariableIndices.take(stageWithoutResults.size).toVector
      freeVariableIndices = freeVariableIndices.drop(stageWithoutResults.size)

      val previousAllocation = currentAllocation
      currentAllocation = currentAllocation ++ stage.zip(newVariableIndices)
      Calculation(stage, previousAllocation, currentAllocation)
    }

  val inputnumber = terms.flatMap(_.inputs).toArray[NNTerm].sorted.zipWithIndex.toMap
  val outputnumber = terms.flatMap(_.outputs).toArray[NNTerm].sorted.zipWithIndex.toMap
  val weightnumber = terms.flatMap(_.weights).toArray[NNTerm].sorted.zipWithIndex.toMap
  val resultnumber = terms.toArray[NNTerm].sorted.zipWithIndex.toMap

  val code = new StringBuilder
  private val classname = s"NNJavaEvaluator${math.abs(terms.hashCode())}"
  private val packagename = "generated.deepnn2"
  private val fullclassname = s"$packagename.$classname"
  code ++= s"""
              |package generated.deepnn2;
              |import net.stoerr.stocklearning.java.deepnn2.AbstractNNJavaEvaluator;
              |public class $classname extends AbstractNNJavaEvaluator {
                                                    |public void run() {
                                                    |   final int id = getGlobalId();
                                                    |   final int inOffset = id*inSubSize;
                                                    |   final int outOffset = id*outSubSize;
                                                    |   final int resOffset = id*resSubSize;
                                        |
                                        |""".stripMargin

  /** We use arrays in for inputs, out for outputs, w for weights, variables memXXXX for intermediate results, res for results. */
  val maxmemlength = calculations.filterNot(_.allocationAfter.isEmpty).map(_.allocationAfter.values.max).max + 1
  code ++= Range(0, maxmemlength).map(i => "float mem" + i + ";").mkString("\n") + "\n"

  for (calculation <- calculations; term <- calculation.terms) {
    def input(term: NNTerm) = if (calculation.allocationBefore.contains(term)) "mem" + calculation.allocationBefore(term)
    else term match {
      case i@I(_) => "in[inOffset + " + inputnumber(i) + "]"
      case o@O(_) => "out[outOffset + " + outputnumber(o) + "]"
      case w@W(_) => "w[" + weightnumber(w) + "]"
      case C(v) => v.toString + "f"
      case other => sys.error("Can't find " + other)
    }
    def result(term: NNTerm) = if (terms.contains(term)) "res[resOffset + " + resultnumber(term) + "]"
    else "mem" + calculation.allocationAfter(term)

    term match {
      case Prod(p1, p2) => code ++= result(term) + " = " + input(p1) + " * " + input(p2) + ";\n"
      case Sum(summands) => code ++= result(term) + " = " + summands.map(input).mkString(" + ") + ";\n"
      case SumProd(summands) => code ++= result(term) + " = " + summands.map(p => input(p._1) + "*" + input(p._2))
        .mkString(" + ") + ";\n"
      case Tanh(t) => code ++= result(term) + " = tanh(" + input(t) + ");\n"
      case RLin(t) => code ++= result(term) + " = " + input(t) + " < 0 ? " + 0 + " : " + input(t) + ";\n"
      case Step(t) => code ++= result(term) + " = " + input(t) + " < 0 ? " + 0 + " : 1;\n"
      case Sqr(t) => code ++= result(term) + " = " + input(t) + "*" + input(t) + ";\n"
      case SoftSign(t) => code ++= result(term) + " = " + input(t) + " / (1 + abs(" + input(t) + "));\n"
      case SoftSignD(t) => code ++= result(term) + " = 1 / ((1 + abs(" + input(t) + ")) * (1 + abs(" + input(t) + "))" +
        ");\n"
      case other => code ++= result(term) + " = " + input(other) + ";\n"
    }
  }

  code ++=
    """
      |    }
      |}
    """.stripMargin

  // code.toString().split("\n").zipWithIndex.foreach(l => println(l._2 + " : " + l._1))
  // println(code)

  private val evaluatorclass: Class[AbstractNNJavaEvaluator] =
    JavaCompilerWithResourceLoad.compile(fullclassname, code.toString())

  /** Don't forget to do .dispose! */
  def makeEvaluator(): AbstractNNJavaEvaluator = evaluatorclass.newInstance()

  println("Subterms: " + terms.flatMap(_.componentStream).size)
  println("Operations: " + calculations.map(_.terms.size).sum)

  println("Transpiler time " + (System.currentTimeMillis() - begintime) + " ms.")
  println(s"${inputnumber.size} inputs, ${outputnumber.size} outputs, ${weightnumber.size} weights, ${resultnumber
    .size} results, $maxmemlength maxmem")

}
