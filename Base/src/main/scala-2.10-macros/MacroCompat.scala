package com.rayrobdod.stringContextParserCombinator

/**
 * Indirect references to methods that changed name between Macro 2.10 and Macro 2.11
 */
object MacroCompat {
	type Context = scala.reflect.macros.Context
	def newTermName(c:Context)(name:String) = c.universe.newTermName(name)
	def newTypeName(c:Context)(name:String) = c.universe.newTypeName(name)
	def stdTermNames(c:Context) = c.universe.nme
	def freshName(c:Context)(prefix:c.universe.TermName):c.universe.TermName = c.fresh(prefix)
	def declaration(c:Context)(t:c.universe.Type)(arg:c.TermName) = t.declaration(arg)
	def paramLists(c:Context)(m:c.universe.MethodSymbol) = m.paramss
	def untypecheck(c:Context)(t:c.universe.Tree) = c.resetAllAttrs(t)
	/** c#eval's name didn't change, but it does require `resetAllAttrs` on 2.10,
	 *  a step which is unnecessary on 2.11 */
	def eval[A](c:Context)(expr:c.Expr[A]):A = {
		val expr1 = c.Expr[A](c.resetAllAttrs(expr.tree.duplicate))
		c.eval(expr1)
	}
}
