package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class OrElse[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	combiner:typelevel.Eithered[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case result:Success[ExprZ, Pos, A] => result.mapValues(combiner.left _)
			case Failure(expectingLeft, Cut.False) => right.parse(input) match {
				case result:Success[ExprZ, Pos, B] => result.mapValues(combiner.right _)
				case Failure(expectingRight, Cut.False) => {
					Failure(expectingLeft ++ expectingRight, Cut.False)
				}
				case failure@Failure(_, Cut.True) => failure
			}
			case failure@Failure(_, Cut.True) => failure
		}
	}
}
