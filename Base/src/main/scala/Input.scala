package com.rayrobdod.stringContextParserCombinator

/**
 * The input to a [[Parser]]
 *
 * @group Input/Result
 */
private[stringContextParserCombinator]
final class Input[+Expr](
	val parts:List[(String, Position)],
	val args:List[Expr],
	argToPosition:Expr => Position
) {
	private[stringContextParserCombinator] def position:Position = {
		if (this.parts(0)._1.length != 0) {
			this.parts(0)._2
		} else if (this.args.nonEmpty) {
			argToPosition(this.args(0))
		} else {
			this.parts(0)._2
		}
	}

	/**
	 *
	 *
	 * @param partsFn called if the next value in the input is a `part`. A `Some` indicates a successful parse.
	 *		The `Int` in the return value is the number of characters in the string that the parser consumes.
	 * @param argsFn called if the next value in the input is an `arg`. A `Some` indicates a successful parse.
	 * @param expecting A textual description of what input the parser will parse successfully
	 */
	private[stringContextParserCombinator] def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:Expr => Option[A],
		expecting: => ExpectingDescription
	):Result[Expr, A] = {
		def failure = Failure(Expecting(expecting, this.position), Cut.False)
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				def success(x:A) = Success(x, new Input(parts.tail, args.tail, argToPosition), Set.empty, Cut.False)
				argsFn(args.head).fold[Result[Expr, A]](failure)(success _)
			} else {
				failure
			}
		} else {
			val (headStr, headPos) = parts.head
			def success(x:(A, Int)) = Success(x._1, new Input((headStr.substring(x._2), headPos + x._2) :: parts.tail, args, argToPosition), Set.empty, Cut.False)
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
