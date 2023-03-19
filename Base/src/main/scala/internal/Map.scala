package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class Map[Expr, A, Z](
	backing:Interpolator[Expr,A], mapping:Function1[A, Z]
) extends Interpolator[Expr, Z] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		backing.interpolate(input).mapValues(mapping)
	}
}
