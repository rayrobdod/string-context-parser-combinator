package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typeclass.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.parse(input) match {
			case successA:Success[ExprZ, Pos, A] => successA.flatMap[ExprZ, Z]({case Success1(valA, restA, expectingA) => right.parse(restA) match {
				case successB:Success[ExprZ, Pos, B] => successB.map[ExprZ, Z]({case Success1(valB, restB, expectingB) => Success1(
					ev.aggregate(valA, valB),
					restB,
					expectingA ++ expectingB
				)})
				case Failure(expectingB) => Failure(
					expectingA ++ expectingB
				)
			}})
			case failure:Failure[Pos] => failure
		}
	}
}
