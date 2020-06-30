package com.rayrobdod.stringContextParserCombinator

import scala.reflect.api.Exprs
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
		def failure = Failure(expecting, this)
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
	private[stringContextParserCombinator] def next:Input.Next = {
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				Input.Arg(args.head)
			} else {
				Input.Eof(parts.head._2)
			}
		} else {
			val (headStr, headPos) = parts.head
			Input.Part(headStr, headPos)
		}
	}
}

private[stringContextParserCombinator] object Input {
        sealed trait Next {
                def description:String
                def position:PositionPoint
        }
        final case class Eof(pos:PositionPoint) extends Next {
                override def description:String = "end of input"
                override def position:PositionPoint = pos
        }
        final case class Part(str:String, pos:PositionPoint) extends Next {
                override def description:String = "\"" + str + "\""
                override def position:PositionPoint = pos
        }
        final case class Arg(expr:Exprs#Expr[_]) extends Next {
                override def description:String = expr.actualType.toString
                override def position:PositionPoint = PositionPoint(expr.tree.pos)
        }
}
