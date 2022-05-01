package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, Parser[Expr, Z]]
) extends AbstractParser[Expr, Z] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, Z] = {
		left.parse(input) match {
			case successLeft@Success(_,_) => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting, leftCut) =>
				right(leftValue).parse(leftRemaining) match {
					case successRight:Success[Expr, Pos, Z] => successRight.map({case Success1(rightValue, rightRemaining, rightExpecting, rightCut) =>
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
