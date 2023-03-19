package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class AndThen[Expr, A, B, Z](
	left:Interpolator[Expr, A],
	right:Interpolator[Expr, B],
	ev:typeclass.Sequenced[A, B, Z]
) extends Interpolator[Expr, Z] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		left.interpolate(input) match {
			case successA:Success[ExprZ, Pos, A] => successA.flatMap[ExprZ, Z]({case Success1(valA, restA, expectingA) => right.interpolate(restA) match {
				case successB:Success[ExprZ, Pos, B] => successB.map[ExprZ, Z]({case Success1(valB, restB, expectingB) => Success1(
					ev.aggregate(valA, valB),
					restB,
					expectingA ++ expectingB
				)})
				case failureB:Failure[Pos] => failureB or expectingA
			}})
			case failure:Failure[Pos] => failure
		}
	}
}
