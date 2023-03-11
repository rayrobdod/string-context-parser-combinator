package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Map[Expr, A, Z](
	backing:Parser[Expr,A], mapping:Function1[A, Z]
) extends Parser[Expr, Z] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
		backing.interpolate(input) match {
			case success:Success[ExprZ, Pos, A] => success.mapValues(mapping)
			case failure:Failure[Pos] => failure
		}
	}
}
