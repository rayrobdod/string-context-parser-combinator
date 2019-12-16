package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.reflect.macros.blackbox.Context

final class Input[U <: Context with Singleton](
	private[stringContextParserCombinator] val parts:List[String],
	private[stringContextParserCombinator] val args:List[U#Expr[Any]]
) {
	def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:U#Expr[Any] => Option[A],
		expecting: => Seq[String]
	):Result[U, A] = {
		def failure = Failure[U](expecting)
		if (parts.head.isEmpty) {
			if (args.nonEmpty) {
				argsFn(args.head).fold[Result[U, A]](failure)(x => Success(x, new Input(parts.tail, args.tail)))
			} else {
				failure
			}
		} else {
			partsFn(parts.head).fold[Result[U, A]](failure)(x => Success(x._1, new Input(parts.head.substring(x._2) :: parts.tail, args)))
		}
	}

	def checkEmpty:Result[U, Unit] = {
		if (List("") == this.parts && args.isEmpty) {
			Success((), this)
		} else {
			Failure(Seq("EOF"))
		}
	}
}

object Input {
	def apply[U <: Context with Singleton](parts:List[String], args:List[U#Expr[Any]]):Input[U] = new Input(parts, args)
}
