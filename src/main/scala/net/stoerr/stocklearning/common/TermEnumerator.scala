package net.stoerr.stocklearning.common

case class TermWithComplexity(complexity: Int, description: String) extends Ordered[TermWithComplexity] {
  private val ordering: Ordering[TermWithComplexity] = Ordering[(Int, String)].on(x => (x.complexity, x.description))

  override def compare(that: TermWithComplexity) = ordering.compare(this, that)
}

class TermFunctionWithComplexity[A, B](complexity: Int, description: String, val func: A => B)
  extends TermWithComplexity(complexity, description)

/**
 * Too lazy to do maths method: we enumerate LOADS of terms and try whether they work.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 18.11.2014
 */
trait TermEnumerator[A] {

  def nullary[B](name: String, ourFunction: A => B) = new TermFunctionWithComplexity[A, B](1, name, ourFunction)

  def unary[B, C](nameprefix: String, ourFunction: B => C)(arg: TermFunctionWithComplexity[A, B]) =
    new TermFunctionWithComplexity[A, C](arg.complexity + 1, nameprefix + " " + arg.description,
      arg.func.andThen(ourFunction))

  def binary[B, C, D](infix: String, ourFunction: (B, C) => D) =
    (arg1: TermFunctionWithComplexity[A, B], arg2: TermFunctionWithComplexity[A, C]) =>
      new TermFunctionWithComplexity[A, D](arg1.complexity + arg2.complexity + 1,
        arg1.description + " " + infix + " " + arg2.description,
        x => ourFunction(arg1.func(x), arg2.func(x)))

  private def mergeTwoStreams[T <: TermWithComplexity](stream1: Stream[T], stream2: Stream[T]): Stream[T] = {
    if (stream1.isEmpty) return stream2
    if (stream2.isEmpty) return stream1
    if (stream1.head <= stream2.head) stream1.head #:: mergeTwoStreams(stream1.tail, stream2)
    else stream2.head #:: mergeTwoStreams(stream1, stream2.tail)
  }

  def mergeStreams[T <: TermWithComplexity](streams: Stream[T]*): Stream[T] =
    streams.reduce(mergeTwoStreams(_, _))

  /** All infinitely many infinite streams are ordered, and the starting point of each stream is in increasing order. */
  private def mergeStreamsStream[T <: TermWithComplexity](streams: Stream[Stream[T]]): Stream[T] = {
    val (first #:: second #:: rest) = streams
    if (first.head <= second.head) first.head #:: mergeStreamsStream(first.tail #:: streams.tail)
    else mergeStreamsStream(mergeStreams(first, second) #:: rest)
  }

  def binaryOpStream[B, C, D](stream1: Stream[TermFunctionWithComplexity[A, B]], stream2: Stream[TermFunctionWithComplexity[A, C]],
                              infix: String, ourFunction: (B, C) => D): Stream[TermFunctionWithComplexity[A, D]] = {
    val op: (TermFunctionWithComplexity[A, B], TermFunctionWithComplexity[A, C]) => TermFunctionWithComplexity[A, D] = binary(infix, ourFunction)
    mergeStreamsStream(stream1.map(s => stream2.map(op(s, _))))
  }


}
