package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class FlatMap[Expr, A, Z](
	left:Parser[Expr, A], right:Function1[A, com.rayrobdod.stringContextParserCombinator.Parser[Expr, Z]]
) extends Parser[Expr, Z] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.interpolate(input) match {
			case successLeft:Success[ExprZ, Pos, A] => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting) =>
				right(leftValue).impl.interpolate(leftRemaining) match {
					case successRight:Success[ExprZ, Pos, Z] => successRight.map({case Success1(rightValue, rightRemaining, rightExpecting) =>
						Success1(rightValue, rightRemaining, leftExpecting ++ rightExpecting)
					})
					case rightFailure:Failure[Pos] => rightFailure or leftExpecting
				}
			})
			case failure:Failure[Pos] => failure
		}
	}
}
