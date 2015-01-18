package net.stoerr.stocklearning.calculationcompiler

import scala.language.implicitConversions

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 16.01.2015
 */
/** gives the possibility to write a calculation with operators */
class CalculationTerm(val calculation: CalculationItem)(implicit calculationTemplate: CalculationTemplate) {
  calculationTemplate += calculation

  def this(variable: CalculationVariable)(implicit calculationTemplate: CalculationTemplate) = this(FactorItem(variable, calculationTemplate.newVariable(), 1))

  def *(o: CalculationTerm) = new CalculationTerm(WeightedSumItem(calculation.output, o.calculation.output, calculationTemplate.newVariable()))

  override def toString = "(" + calculation + ")"
}

object CalculationTerm {
  implicit def toTerm(v: CalculationVariable)(implicit calculationTemplate: CalculationTemplate)
  = calculationTemplate.toTerm(v)
}

// XXX was ist mit zusätzlichen Inputs?
// object with inputs, outputs, nebeninputs
abstract class CalculationMaker(val inputCount: Int, val outputCount: Int) {
  def apply(inputs: Seq[CalculationVariable])(outputs: Seq[CalculationVariable])(implicit tmpl: CalculationTemplate): Unit

  def |(o: CalculationMaker) = new CalculationMaker(inputCount, o.outputCount) {
    require(this.outputCount == o.inputCount)

    override def apply(inputs: Seq[CalculationVariable])(outputs: Seq[CalculationVariable])(implicit tmpl: CalculationTemplate): Unit = {
      val intermediateVars = (0 until outputCount).map(_ => tmpl.newVariable())
      this(inputs)(intermediateVars)
      o(intermediateVars)(outputs)
    }
  }
}
