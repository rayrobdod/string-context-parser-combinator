package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * The input to a {@link Parser}
 *
 * @group Input/Result
 */
final class Input[U <: Context with Singleton](
	parts:List[(String, PositionPoint)],
	args:List[U#Expr[Any]]
) {
	private[stringContextParserCombinator] def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:U#Expr[Any] => Option[A],
		expecting: => Failure.Expecting
	):Result[U, A] = {
		def failure = Failure(this.next, expecting)
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				def success(x:A) = Success(x, new Input(parts.tail, args.tail))
				argsFn(args.head).fold[Result[U, A]](failure)(success _)
			} else {
				failure
			}
		} else {
			val (headStr, headPos) = parts.head
			def success(x:(A, Int)) = Success(x._1, new Input((headStr.substring(x._2), headPos + x._2) :: parts.tail, args))
			partsFn(headStr).fold[Result[U, A]](failure)(success _)
		}
	}

	/**
	 * Returns a {@link Success} if this Input is empty; otherwise a
	 * {@link Failure}
	 */
	private[stringContextParserCombinator] def isEmpty:Boolean = parts.head._1.isEmpty && args.isEmpty

	/**
	 * Reports the next symbol, but only in a form suitable for the Failure's found parameter
	 */
	private[stringContextParserCombinator] def next:(String, PositionPoint) = {
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				(args.head.actualType.toString, PositionPoint(args.head.tree.pos))
			} else {
				("EOF", parts.head._2)
			}
		} else {
			val (headStr, headPos) = parts.head
			("\"" + headStr + "\"", headPos)
		}
	}
}
