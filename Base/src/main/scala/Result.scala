package com.rayrobdod.stringContextParserCombinator

import scala.reflect.api.Exprs
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * The result of a parse
 * @group Input/Result
 */
sealed trait Result[+Expr, +A] {
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
final case class Success[+Expr, +A](
	val value:A,
	val remaining:Input[Expr]
) extends Result[Expr, A]

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param found the value that was found
 * @param expecting what the parser was expecting
 */
final case class Failure[+Expr](
	val expecting:Failure.Expecting,
	val remaining:Input[Expr]
) extends Result[Expr, Nothing] {
	private[stringContextParserCombinator] def msg(implicit ev:Expr <:< Exprs#Expr[_]):String = s"Found ${remaining.description} ; Expected $expecting"
	def report(c:Context)(implicit ev:Expr <:< c.Expr[_]):Nothing = {
		c.abort(remaining.position.cast(c), msg)
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
