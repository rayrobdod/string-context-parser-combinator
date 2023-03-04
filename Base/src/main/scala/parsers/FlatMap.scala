package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, com.rayrobdod.stringContextParserCombinator.Parser[Expr, Z]]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case successLeft:Success[ExprZ, Pos, A] => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting) =>
				right(leftValue).impl.parse(leftRemaining) match {
					case successRight:Success[ExprZ, Pos, Z] => successRight.map({case Success1(rightValue, rightRemaining, rightExpecting) =>
						Success1(rightValue, rightRemaining, leftExpecting ++ rightExpecting)
					})
					case Failure(rightExpecting) => Failure(leftExpecting ++ rightExpecting)
				}
			})
			case failure:Failure[Pos] => failure
		}
	}
}
