package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class DelayedConstruction[Expr, A](
	backing:Function0[Parser[Expr, A]]
) extends AbstractParser[Expr, A] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, A] = {
		backing.apply().parse(input)
	}
}
