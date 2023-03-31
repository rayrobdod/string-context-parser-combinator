package com.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context

/**
 * Associates an Expr type with the implicit types required to lift a value into an Expr.
 * Support for [[Interpolator.mapToExpr]].
 */
trait ToExprMapping[Expr[_], ToExpr[_], Type[_]] {
	def apply[A](value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A]
}

/** Predefined implicit implementations of ToExprMapping */
object ToExprMapping {
	def toExprContext(c:Context):ToExprMapping[c.Expr, c.universe.Liftable, c.TypeTag] = {
		new ToExprMapping[c.Expr, c.universe.Liftable, c.TypeTag] {
			def apply[A](value:A, fn:c.universe.Liftable[A], tpe: c.TypeTag[A]):c.Expr[A] = {
				c.Expr[A](fn(value))(tpe)
			}
		}
	}
}
