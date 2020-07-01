package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class DelayedConstruction[U <: Context with Singleton, A](
	backing:Function0[Parser[U, A]]
) extends AbstractParser[U, A] {
	def parse(input:Input[U#Expr[_]]):Result[U#Expr[_], A] = {
		backing.apply.parse(input)
	}
}
