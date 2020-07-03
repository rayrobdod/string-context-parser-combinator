package com.rayrobdod.stringContextParserCombinatorExample.datetime

/**
 * Indirect references to methods that changed name between Macro 2.10 and Macro 2.11
 */
object MacroCompat {
	type Context = scala.reflect.macros.blackbox.Context
	def newTermName(c:Context)(name:String) = c.universe.TermName(name)
	def stdTermNames(c:Context) = c.universe.termNames
}
