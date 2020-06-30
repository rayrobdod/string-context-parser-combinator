package com.rayrobdod.stringContextParserCombinator

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * The result of a parse
 * @group Input/Result
 */
sealed trait Result[+U <: Context with Singleton, +A] {
}

/**
 * The result of a successful parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param value the parsed value
 * @param remaining input that was not consumed by the parser
 */
final case class Success[U <: Context with Singleton, +A](
	val value:A,
	val remaining:Input[U]
) extends Result[U, A]

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param found the value that was found
 * @param expecting what the parser was expecting
 */
final case class Failure[U <: Context with Singleton](
	val expecting:Failure.Expecting,
	val remaining:Input[U]
) extends Result[U, Nothing] {
	private[this] def found = remaining.next
	private[stringContextParserCombinator] def msg:String = s"Found ${found._1} ; Expected $expecting"
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
