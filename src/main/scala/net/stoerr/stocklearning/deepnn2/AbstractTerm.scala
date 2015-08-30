package net.stoerr.stocklearning.deepnn2

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 28.08.2015
 */
sealed trait AbstractTerm[T] {

  def +(o: AbstractTerm[T]): AbstractTerm[T] = new AbstractSum[T](this, o)

  def -(o: AbstractTerm[T]): AbstractTerm[T] = new AbstractSum[T](this, AbstractProduct[T](o, AbstractConstant[T](-1)))

  def *(o: AbstractTerm[T]): AbstractTerm[T] = new AbstractProduct[T](this, o)

}

case class AbstractConstant[T](value: Double) extends AbstractTerm[T] {
  override def toString = "" + value
}

case class AbstractSum[T](s1: AbstractTerm[T], s2: AbstractTerm[T]) extends AbstractTerm[T] {
  override def toString = s1 + " + " + s2
}

case class AbstractProduct[T](s1: AbstractTerm[T], s2: AbstractTerm[T]) extends AbstractTerm[T] {
  override def toString = s1 + " + " + s2
}

