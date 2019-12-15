package com.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

private[stringContextParserCombinator] object Eval {
	def simple[A](c:Context)(expr:c.Expr[A]):A = {
		c.eval(expr)
	}
}
