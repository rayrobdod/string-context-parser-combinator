package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Attempt[Expr, A](
		backing:Parser[Expr, A]
) extends Parser[Expr, A] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		backing.parse(input) match {
			case success:Success[ExprZ, Pos, A] => success
			case Failure(ExpectingSet.Empty(), _) => Failure(ExpectingSet.Empty(), Cut.False)
			// TODO: not completely loose context of expecting descriptions
			case Failure(ExpectingSet.NonEmpty(_, descs), _) => Failure(ExpectingSet.NonEmpty(input.position, descs), Cut.False)
		}
	}
}
