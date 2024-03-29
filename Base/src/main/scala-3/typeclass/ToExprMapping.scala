package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.ClassTag

/**
 * Associates an Expr type with the implicit types required to lift a value into an Expr.
 * Support for [[Interpolator.mapToExpr]]
 */
trait ToExprMapping[Expr[_], ToExpr[_], Type[_]] {
	def apply[A](value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A]
}

/** Predefined implicit implementations of ToExprMapping */
object ToExprMapping {
	import quoted._
	given forQuoted(using Quotes):ToExprMapping[Expr, ToExpr, Type] = {
		new ToExprMapping[Expr, ToExpr, Type] {
			def apply[A](value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A] = {
				Expr[A](value)(using fn)
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
