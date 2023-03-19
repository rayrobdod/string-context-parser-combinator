package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Map {
	def interpolator[Expr, A, Z](
		backing:Interpolator[Expr, A],
		mapping: A => Z
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapValues(mapping)
			}
		}
	}
}
