package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, com.rayrobdod.stringContextParserCombinator.Parser[Expr, Z]]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case successLeft@Success(_,_) => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting, leftCut) =>
				right(leftValue).impl.parse(leftRemaining) match {
					case successRight:Success[ExprZ, Pos, Z] => successRight.map({case Success1(rightValue, rightRemaining, rightExpecting, rightCut) =>
						Success1(rightValue, rightRemaining, leftExpecting ++ rightExpecting, leftCut | rightCut)
					})
					case Failure(rightExpecting, Cut.False) => Failure(leftExpecting ++ rightExpecting, leftCut)
					case failure@Failure(_, Cut.True) => failure
				}
			})
			case failure@Failure(_, _) => failure
		}
	}
}
