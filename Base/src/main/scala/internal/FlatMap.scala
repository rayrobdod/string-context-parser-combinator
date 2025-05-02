package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object FlatMap {
	def interpolator[Ctx, Expr, A, Z](
		left:Interpolator[Ctx, Expr, A],
		right:(A, Ctx) => name.rayrobdod.stringContextParserCombinator.Interpolator[Ctx, Expr, Z]
	):Interpolator[Ctx, Expr, Z] = {
		new Interpolator[Ctx, Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				left.interpolate(input) match {
					case successLeft:Success[ExprZ, Pos, A] => successLeft.flatMap({case Success1(leftValue, leftRemaining, leftExpecting) =>
						right(leftValue, ctx).impl.interpolate(leftRemaining) match {
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
