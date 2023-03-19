package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object DelayedConstruction {
	def interpolator[Expr, A](
		backing:() => com.rayrobdod.stringContextParserCombinator.Interpolator[Expr, A]
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.apply().impl.interpolate(input)
			}
		}
	}
}
