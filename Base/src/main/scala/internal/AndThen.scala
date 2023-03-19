package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object AndThen {
	def interpolator[Expr, A, B, Z](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Sequenced[A, B, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				AndThen.parse(
					{(x:Input[ExprZ, Pos]) => left.interpolate(x)},
					{(x:Input[ExprZ, Pos]) => right.interpolate(x)},
					combiner.aggregate _,
					input
				)
			}
		}
	}

	private def parse[Expr, A, B, Z, Pos](
		useLeft:Input[Expr, Pos] => Result[Expr, Pos, A],
		useRight:Input[Expr, Pos] => Result[Expr, Pos, B],
		combiner:(A, B) => Z,
		input:Input[Expr, Pos]
	):Result[Expr, Pos, Z] = {
		useLeft(input) match {
			case successA:Success[Expr, Pos, A] => successA.flatMap[Expr, Z]({case Success1(valA, restA, expectingA) => useRight(restA) match {
				case successB:Success[Expr, Pos, B] => successB.map[Expr, Z]({case Success1(valB, restB, expectingB) => Success1(
					combiner(valA, valB),
					restB,
					expectingA ++ expectingB
				)})
				case failureB:Failure[Pos] => failureB or expectingA
			}})
			case failure:Failure[Pos] => failure
		}
	}
}
