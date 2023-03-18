package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class OrElse[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	combiner:typeclass.Eithered[A, B, Z]
) extends Parser[Expr, Z] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.interpolate(input) match {
			case leftSuccess:Success[ExprZ, Pos, A] => leftSuccess.mapValues(combiner.left _)
			case leftFailure:Failure[Pos] => {
				if (leftFailure.isPositionGt(input.position)) {
					// consumed input; don't try the other branch
					leftFailure
				} else {
					// assume that ExpectingSet.Empty means no input consumed
					right.interpolate(input) match {
						case rightSuccess:Success[ExprZ, Pos, B] => rightSuccess.mapValues(combiner.right _)
						case rightFailure:Failure[Pos] => leftFailure or rightFailure
					}
				}
			}
		}
	}
}
