package com.rayrobdod

import scala.reflect.macros.blackbox.Context

/**
 * A parser combinator for StringContext handling
 */
package object stringContextParserCombinator {
	type Data[U <: Context with Singleton] = (List[String], List[U#Expr[Any]])
	final case class CodePoint(val value:Int) extends AnyVal
}
