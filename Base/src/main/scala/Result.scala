package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set

/**
 * The result of a parse
 * @group Input/Result
 */
private[stringContextParserCombinator]
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
 * @param expecting a description of what an already-run parser could find next and still succeed
 * @param isCut true if other OrElse branches should be ignored
 */
private[stringContextParserCombinator]
final case class Success[+Expr, +A](
	val value:A,
	val remaining:Input[Expr],
	val expecting:Set[Expecting],
	val isCut:Cut
) extends Result[Expr, A] {
	private[stringContextParserCombinator]
	def map[Z](fn:A => Z):Success[Expr, Z] = Success(fn(value), remaining, expecting, isCut)
}

/**
 * The result of a failed parse
 *
 * @group Input/Result
 *
 * @constructor
 * @param expecting a description of what the parser was expecting to find
 * @param isCut true if other OrElse branches should be ignored
 */
private[stringContextParserCombinator]
final case class Failure(
	val expecting:Set[Expecting],
	val isCut:Cut
) extends Result[Nothing, Nothing] {
}

private[stringContextParserCombinator]
object Failure {
	def apply(expecting:Expecting, isCut:Cut):Failure = Failure(Set(expecting), isCut)
}
