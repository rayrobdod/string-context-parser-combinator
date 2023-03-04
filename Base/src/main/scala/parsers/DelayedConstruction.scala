package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class DelayedConstruction[Expr, A](
	backing:Function0[com.rayrobdod.stringContextParserCombinator.Parser[Expr, A]]
) extends Parser[Expr, A] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		backing.apply().impl.parse(input)
	}
}
