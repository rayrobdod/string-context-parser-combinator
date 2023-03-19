package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object OrElse {
	def interpolator[Expr, A, B, Z](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Eithered[A, B, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				OrElse.interpolate(left, right, combiner, input)
			}
		}
	}

	private def interpolate[Expr, A, B, Z, Pos](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Eithered[A, B, Z],
		input:Input[Expr, Pos])(
		implicit ev1:Ordering[Pos]
	):Result[Expr, Pos, Z] = {
		left.interpolate(input) match {
			case leftSuccess:Success[Expr, Pos, A] => leftSuccess.mapValues(combiner.left _)
			case leftFailure:Failure[Pos] => {
				if (leftFailure.isPositionGt(input.position)) {
					// consumed input; don't try the other branch
					leftFailure
				} else {
					// assume that ExpectingSet.Empty means no input consumed
					right.interpolate(input) match {
						case rightSuccess:Success[Expr, Pos, B] => rightSuccess.mapValues(combiner.right _)
						case rightFailure:Failure[Pos] => leftFailure or rightFailure
					}
				}
			}
		}
	}
}
