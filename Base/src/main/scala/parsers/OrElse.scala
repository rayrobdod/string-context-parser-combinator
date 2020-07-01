package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers]
final class OrElse[Expr, A](
	left:Parser[Expr, A],
	right:Parser[Expr, A]
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		left.parse(input) match {
			case Success(v, r) => Success(v, r)
			case Failure(expect1, remain1) => right.parse(input) match {
				case Success(v, r) => Success(v, r)
				case Failure(expect2, remain2) => {
					Failure(Failure.Leaf("TODO OrElse Error Reporting"), input)
				}
			}
		}
	}
}
