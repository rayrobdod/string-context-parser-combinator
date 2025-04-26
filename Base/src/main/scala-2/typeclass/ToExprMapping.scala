package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context
import scala.reflect.ClassTag

/**
 * Associates an Expr type with the implicit types required to lift a value into an Expr.
 * Support for [[Interpolator.mapToExpr]].
 */
trait ToExprMapping[Expr[+_], ToExpr[_], Type[_]] {
	def apply[A](value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A]
}

/** Predefined implicit implementations of ToExprMapping */
object ToExprMapping {
	def forContext(c:Context):ToExprMapping[c.Expr, c.universe.Liftable, c.TypeTag] = {
		new ToExprMapping[c.Expr, c.universe.Liftable, c.TypeTag] {
			def apply[A](value:A, fn:c.universe.Liftable[A], tpe: c.TypeTag[A]):c.Expr[A] = {
				c.Expr[A](fn(value))(tpe)
			}
		}
	}

	def forId:ToExprMapping[Id, IdToExpr, ClassTag] = {
		new ToExprMapping[Id, IdToExpr, ClassTag] {
			def apply[A](value:A, fn: IdToExpr[A], tpe: ClassTag[A]):Id[A] = {
				value
			}
		}
	}
}
