package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * The result of a parse
 * @group Result
 */
sealed trait Result[+U <: Context with Singleton, +A] {
}

/**
 * @group Result
 */
final case class Success[U <: Context with Singleton, +A](
	val value:A,
	val remaining:Input[U]
) extends Result[U, A]

/**
 * @group Result
 */
final case class Failure(
	val found:(String, PositionPoint),
	val expecting:Failure.Expecting
) extends Result[Nothing, Nothing] {
	def msg:String =  s"Found ${found._1} ; Expected $expecting"
	def report(c:Context):Nothing = {
		c.abort(found._2.cast(c), msg)
	}
}

object Failure {
	sealed trait Expecting
	final case class Leaf(x:String) extends Expecting {
		override def toString:String = x
	}
	final case class Or(options:Seq[Expecting]) extends Expecting {
		override def toString:String = options.mkString(" | ")
	}
}
