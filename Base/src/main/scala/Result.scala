package com.rayrobdod.stringContextParserCombinator

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
) extends Result[Expr, A] {
	private[stringContextParserCombinator]
	def map[Z](fn:A => Z):Success[Expr, Z] = Success(fn(value), remaining, trace, isCut)
}

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param trace a trace of the parsers that lead to this result
 * @param isCut true if other OrElse branches should be ignored
 */
final case class Failure[+Expr](
	trace:Trace[Expr],
	isCut:Cut
) extends Result[Expr, Nothing]
