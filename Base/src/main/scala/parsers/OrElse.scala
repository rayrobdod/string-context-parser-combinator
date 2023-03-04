package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class OrElse[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	combiner:typeclass.Eithered[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case leftSuccess:Success[ExprZ, Pos, A] => leftSuccess.mapValues(combiner.left _)
			case leftFailure:Failure[Pos] => {
				if (leftFailure.isPositionGt(input.position)) {
					// consumed input; don't try the other branch
					leftFailure
				} else {
					// assume that ExpectingSet.Empty means no input consumed
					right.parse(input) match {
						case rightSuccess:Success[ExprZ, Pos, B] => rightSuccess.mapValues(combiner.right _)
						case rightFailure:Failure[Pos] => leftFailure or rightFailure
					}
				}
			}
		}
	}
}
