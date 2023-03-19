package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class DelayedConstruction[Expr, A](
	backing:Function0[com.rayrobdod.stringContextParserCombinator.Interpolator[Expr, A]]
) extends Interpolator[Expr, A] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		backing.apply().impl.interpolate(input)
	}
}
