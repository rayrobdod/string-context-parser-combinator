package com.rayrobdod.stringContextParserCombinator

/**
 * The input to a [[Parser]]
 *
 * @group Input/Result
 */
final class Input[+Expr](
	private[stringContextParserCombinator] val parts:List[(String, Position)],
	private[stringContextParserCombinator] val args:List[Expr]
) {
	private[stringContextParserCombinator] def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:Expr => Option[A],
		expecting: => ExpectingDescription
	):Result[Expr, A] = {
		val trace = LeafTrace(expecting, this)
		def failure = Failure(trace, Cut.False)
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				def success(x:A) = Success(x, new Input(parts.tail, args.tail), trace, Cut.False)
				argsFn(args.head).fold[Result[Expr, A]](failure)(success _)
			} else {
				failure
			}
		} else {
			val (headStr, headPos) = parts.head
			def success(x:(A, Int)) = Success(x._1, new Input((headStr.substring(x._2), headPos + x._2) :: parts.tail, args), trace, Cut.False)
			partsFn(headStr).fold[Result[Expr, A]](failure)(success _)
		}
	}

	/**
	 * Returns a {@link Success} if this Input is empty; otherwise a
	 * {@link Failure}
	 */
	private[stringContextParserCombinator] def isEmpty:Boolean = parts.head._1.isEmpty && args.isEmpty

	override def toString:String = s"Input(${parts}, ${args})"
	override def hashCode:Int = java.util.Objects.hash(parts, args)
	override def equals(rhs:Any):Boolean = rhs match {
		case other:Input[_] => this.parts == other.parts && this.args == other.args
		case _ => false
	}
}
