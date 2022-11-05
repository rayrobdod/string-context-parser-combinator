package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThenWithCut[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typeclass.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case successA:Success[ExprZ, Pos, A] => successA.flatMap[ExprZ, Z]({case Success1(valA, restA, expectingA@_, cutA@_) => right.parse(restA) match {
				case successB:Success[ExprZ, Pos, B] => successB.map[ExprZ, Z]({case Success1(valB, restB, expectingB, cutB@_) => Success1(
					ev.aggregate(valA, valB),
					restB,
					expectingB,
					Cut.True
				)})
				case Failure(expectingB, cutB@_) => Failure(
					expectingB,
					Cut.True
				)
			}})
			case failure@Failure(_, _) => failure
		}
	}
}
