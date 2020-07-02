package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, Parser[Expr, Z]]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(leftValue, leftRemaining, leftTrace) => right(leftValue).parse(leftRemaining) match {
				case Success(rightValue, rightRemaining, rightTrace) => Success(rightValue, rightRemaining, ThenTrace(leftTrace, rightTrace))
				case Failure(rightTrace) => Failure(ThenTrace(leftTrace, rightTrace))
			}
			case failure@Failure(_) => failure
		}
	}
}
