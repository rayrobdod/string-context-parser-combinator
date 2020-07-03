package com.rayrobdod.stringContextParserCombinator

import scala.reflect.api.Exprs

/**
 * The input to a {@link Parser}
 *
 * @group Input/Result
 */
final class Input[+Expr](
	private val parts:List[(String, PositionPoint)],
	private val args:List[Expr]
) {
	private[stringContextParserCombinator] def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:Expr => Option[A],
		expecting: => Expecting
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

	/**
	 * Returns a string representation of this input, suitable for printing to a users
	 */
	private[stringContextParserCombinator] def description(implicit ev:Expr <:< Exprs#Expr[_]):String = {
		if (this.isEmpty) {
			"end of input"
		} else {
			scala.collection.immutable.Range(0, args.size)
				.map(i => s"${parts(i)._1}$${${ev(args(i)).tree}}")
				.mkString("\"", "", parts(args.size)._1 + "\"")
		}
	}

	/**
	 * Returns the position of this input
	 */
	private[stringContextParserCombinator] def position(implicit ev:Expr <:< Exprs#Expr[_]):PositionPoint = {
		if (parts(0)._1.length != 0) {
			parts(0)._2
		} else if (args.nonEmpty) {
			PositionPoint(ev(args(0)).tree.pos)
		} else {
			parts(0)._2
		}
	}

	override def toString:String = s"Input(${parts}, ${args})"
	override def hashCode:Int = java.util.Objects.hash(parts, args)
	override def equals(rhs:Any):Boolean = rhs match {
		case other:Input[_] => this.parts == other.parts && this.args == other.args
		case _ => false
	}
}
