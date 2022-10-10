package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Map[Expr, A, Z](
	backing:Parser[Expr,A], mapping:Function1[A, Z]
) extends Parser[Expr, Z] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Z] = {
		backing.parse(input) match {
			case success:Success[ExprZ, Pos, A] => success.mapValues(mapping)
			case failure:Failure[Pos] => failure
		}
	}
}
