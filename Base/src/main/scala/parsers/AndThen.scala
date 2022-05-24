package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case successA:Success[ExprZ, Pos, A] => successA.flatMap[ExprZ, Z]({case Success1(valA, restA, expectingA, cutA) => right.parse(restA) match {
				case successB:Success[ExprZ, Pos, B] => successB.map[ExprZ, Z]({case Success1(valB, restB, expectingB, cutB) => Success1(
					ev.aggregate(valA, valB),
					restB,
					if (cutB.toBoolean) {expectingB} else {expectingA ++ expectingB},
					cutA | cutB
				)})
				case Failure(expectingB, cutB) => Failure(
					if (cutB.toBoolean) {expectingB} else {expectingA ++ expectingB},
					cutA | cutB
				)
			}})
			case failure@Failure(_,_) => failure
		}
	}
}
