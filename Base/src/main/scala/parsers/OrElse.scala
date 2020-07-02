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
			case Failure(traceLeft) => right.parse(input) match {
				case result:Success[Expr, A] => result
				case Failure(traceRight) => {
					Failure(OrTrace(traceLeft, traceRight))
				}
			}
		}
	}
}
