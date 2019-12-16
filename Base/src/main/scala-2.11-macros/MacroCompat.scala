package com.rayrobdod.stringContextParserCombinator

/**
 * Indirect references to methods that changed name between Macro 2.10 and Macro 2.11
 */
object MacroCompat {
	type Context = scala.reflect.macros.blackbox.Context
	def newTermName(c:Context)(name:String) = c.universe.TermName(name)
	def newTypeName(c:Context)(name:String) = c.universe.TypeName(name)
	def stdTermNames(c:Context) = c.universe.termNames
	def declaration(c:Context)(t:c.universe.Type)(arg:c.TermName) = t.decl(arg)
	def paramLists(c:Context)(m:c.universe.MethodSymbol) = m.paramLists
	def untypecheck(c:Context)(t:c.universe.Tree) = c.untypecheck(t)
	/** c#eval's name didn't change, but it does require `resetAllAttrs` on 2.10,
	 *  a step which is unnecessary on 2.11 */
	def eval[A](c:Context)(expr:c.Expr[A]):A = {
		c.eval(expr)
	}
}
