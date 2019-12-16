package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

final class Input[U <: Context with Singleton](
	private[stringContextParserCombinator] val parts:List[(String, PositionPoint)],
	private[stringContextParserCombinator] val args:List[U#Expr[Any]]
) {
	def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:U#Expr[Any] => Option[A],
		expecting: => Failure.Expecting
	):Result[U, A] = {
		def failure = Failure[U](this.next, expecting)
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
	def isEmpty:Boolean = parts.head._1.isEmpty && args.isEmpty

	/**
	 * Reports the next symbol, but only in a form suitable for the Failure's found parameter
	 */
	def next:(String, PositionPoint) = {
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

object Input {
	def apply[U <: Context with Singleton](parts:List[(String, PositionPoint)], args:List[U#Expr[Any]]):Input[U] = new Input(parts, args)
}
