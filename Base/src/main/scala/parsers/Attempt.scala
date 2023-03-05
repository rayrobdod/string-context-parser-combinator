package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Attempt[Expr, A](
		backing:Parser[Expr, A]
) extends Parser[Expr, A] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		backing.parse(input) match {
			case success:Success[ExprZ, Pos, A] => success
			case Failure(_, expecting) => Failure(Option(input.position), expecting)
		}
	}
}
