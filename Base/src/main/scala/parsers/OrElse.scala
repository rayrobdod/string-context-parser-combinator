package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers]
final class OrElse[Expr, A](
	left:Parser[Expr, A],
	right:Parser[Expr, A]
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		left.parse(input) match {
			case result:Success[Expr, A] => result
			case Failure(traceLeft, Cut.False) => right.parse(input) match {
				case result:Success[Expr, A] => result
				case Failure(traceRight, Cut.False) => {
					Failure(OrTrace(traceLeft, traceRight), Cut.False)
				}
				case failure@Failure(_, Cut.True) => failure
			}
			case failure@Failure(_, Cut.True) => failure
		}
	}
}
