package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class DelayedConstruction[Expr, A](
	backing:Function0[Parser[Expr, A]]
) extends AbstractParser[Expr, A] {
	def parse(input:Input[Expr]):Result[Expr, A] = {
		backing.apply.parse(input)
	}
}
