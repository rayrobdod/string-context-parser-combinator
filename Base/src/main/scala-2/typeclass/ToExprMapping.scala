package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.macros.blackbox.Context
import scala.reflect.ClassTag

/**
 * Associates an Expr type with the implicit types required to lift a value into an Expr.
 * Support for [[Interpolator.mapToExpr]].
 */
trait ToExprMapping[Ctx, Expr[+_], ToExpr[_], Type[_]] {
	def apply[A](ctx:Ctx, value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A]
}

/** Predefined implicit implementations of ToExprMapping */
object ToExprMapping {
	def forContext(c:Context):ToExprMapping[c.type, c.Expr, c.universe.Liftable, c.TypeTag] = {
		new ToExprMapping[c.type, c.Expr, c.universe.Liftable, c.TypeTag] {
			def apply[A](ctx:c.type, value:A, fn:c.universe.Liftable[A], tpe: c.TypeTag[A]):c.Expr[A] = {
				c.Expr[A](fn(value))(tpe)
			}
		}
	}

	def forId:ToExprMapping[IdCtx, Id, IdToExpr, ClassTag] = {
		new ToExprMapping[IdCtx, Id, IdToExpr, ClassTag] {
			def apply[A](ctx:IdCtx, value:A, fn: IdToExpr[A], tpe: ClassTag[A]):Id[A] = {
				value
			}
		}
	}
}
