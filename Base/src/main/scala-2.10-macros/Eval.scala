package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.Context

private[stringContextParserCombinator] object Eval {
	def simple[A](c:Context)(expr:c.Expr[A]):A = {
		val expr1 = c.Expr[A](c.resetAllAttrs(expr.tree.duplicate))
		c.eval(expr1)
	}
}
