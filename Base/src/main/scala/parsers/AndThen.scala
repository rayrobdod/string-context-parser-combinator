package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(valA, restA, expectingA, cutA) => right.parse(restA) match {
				case Success(valB, restB, expectingB, cutB) => Success(
					ev.aggregate(valA, valB),
					restB,
					if (cutB.toBoolean) {expectingB} else {expectingA ++ expectingB},
					cutA | cutB
				)
				case Failure(expectingB, cutB) => Failure(
					if (cutB.toBoolean) {expectingB} else {expectingA ++ expectingB},
					cutA | cutB
				)
			}
			case failure@Failure(_,_) => failure
		}
	}
}
