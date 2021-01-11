package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class OrElse[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	combiner:typelevel.Eithered[A, B, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case result:Success[Expr, A] => result.map(combiner.left _)
			case Failure(traceLeft, Cut.False) => right.parse(input) match {
				case result:Success[Expr, B] => result.map(combiner.right _)
				case Failure(traceRight, Cut.False) => {
					Failure(OrTrace(traceLeft, traceRight), Cut.False)
				}
				case failure@Failure(_, Cut.True) => failure
			}
			case failure@Failure(_, Cut.True) => failure
		}
	}
}
