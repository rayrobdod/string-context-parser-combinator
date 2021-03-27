package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThenWithCut[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(valA, restA, _, _) => right.parse(restA) match {
				case Success(valB, restB, expectingB, _) => Success(
					ev.aggregate(valA, valB),
					restB,
					expectingB,
					Cut.True
				)
				case Failure(expectingB, _) => Failure(
					expectingB,
					Cut.True
				)
			}
			case failure@Failure(_, _) => failure
		}
	}
}
