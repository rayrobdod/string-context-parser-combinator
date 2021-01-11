package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, Parser[Expr, Z]]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(leftValue, leftRemaining, leftTrace, leftCut) => right(leftValue).parse(leftRemaining) match {
				case Success(rightValue, rightRemaining, rightTrace, rightCut) => Success(rightValue, rightRemaining, ThenTrace(leftTrace, rightTrace), leftCut | rightCut)
				case Failure(rightTrace, Cut.False) => Failure(ThenTrace(leftTrace, rightTrace), leftCut)
				case failure@Failure(_, Cut.True) => failure
			}
			case failure@Failure(_, _) => failure
		}
	}
}
