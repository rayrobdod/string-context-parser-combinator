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
 * @param trace a trace of the parsers that lead to this result
 * @param isCut true if other OrElse branches should be ignored
 */
final case class Success[+Expr, +A](
	val value:A,
	val remaining:Input[Expr],
	val trace:Trace[Expr],
	val isCut:Cut
) extends Result[Expr, A]

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param trace a trace of the parsers that lead to this result
 * @param isCut true if other OrElse branches should be ignored
 */
final case class Failure[+Expr](trace:Trace[Expr], isCut:Cut) extends Result[Expr, Nothing] {
	private def remainingDescription(implicit ev:Expr <:< Exprs#Expr[_]):String = {
		trace
			.removeRequiredThens
			.leftMostRemaining
			.description
	}

	private def remainingPosition(implicit ev:Expr <:< Exprs#Expr[_]):PositionPoint = {
		trace
			.removeRequiredThens
			.leftMostRemaining
			.position
	}

	private def expectingDescription:String = {
		trace
			.removeRequiredThens
			.removeEmptyTraces
			.expectingDescription
	}

	def report(c:Context)(implicit ev:Expr <:< c.Expr[_]):Nothing = {
		c.abort(remainingPosition.cast(c), s"Found ${remainingDescription} ; Expected ${expectingDescription}")
	}
}
