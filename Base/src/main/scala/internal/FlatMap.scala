package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object FlatMap {
	def interpolator[Expr, A, Z](
		left:Interpolator[Expr, A],
		right:A => name.rayrobdod.stringContextParserCombinator.Interpolator[Expr, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				left.interpolate(input) match {
					case successLeft:Success[ExprZ, Pos, A] => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting) =>
						right(leftValue).impl.interpolate(leftRemaining) match {
							case successRight:Success[ExprZ, Pos, Z] => successRight.map({case Success1(rightValue, rightRemaining, rightExpecting) =>
								Success1(rightValue, rightRemaining, leftExpecting ++ rightExpecting)
							})
							case rightFailure:Failure[Pos] => rightFailure or leftExpecting
						}
					})
					case failure:Failure[Pos] => failure
				}
			}
		}
	}
}
