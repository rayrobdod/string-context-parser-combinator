package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, Z] = {
		left.parse(input) match {
			case successA:Success[Expr, Pos, A] => successA.flatMap[Expr, Z]({case Success1(valA, restA, expectingA, cutA) => right.parse(restA) match {
				case successB:Success[Expr, Pos, B] => successB.map[Expr, Z]({case Success1(valB, restB, expectingB, cutB) => Success1(
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
