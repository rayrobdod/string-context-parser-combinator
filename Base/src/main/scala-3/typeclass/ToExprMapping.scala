package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.ClassTag

/**
 * Associates an Expr type with the implicit types required to lift a value into an Expr.
 * Support for [[Interpolator.mapToExpr]]
 */
trait ToExprMapping[Ctx, Expr[+_], ToExpr[_], Type[_]] {
	def apply[A](ctx:Ctx, value:A, fn:ToExpr[A], tpe: Type[A]):Expr[A]
}

/** Predefined implicit implementations of ToExprMapping */
object ToExprMapping {
	import quoted._
	given forQuoted:ToExprMapping[Quotes, Expr, ToExpr, TypeCreator] = {
		new ToExprMapping[Quotes, Expr, ToExpr, TypeCreator] {
			def apply[A](ctx:Quotes, value:A, fn:ToExpr[A], tpe: TypeCreator[A]):Expr[A] = {
				Expr[A](value)(using fn)(using ctx)
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
