package name.rayrobdod.stringContextParserCombinator

import name.rayrobdod.stringContextParserCombinator.typeclass.Exprs

/**
 * A partial function which is valid according to an `Expr[Boolean]` instead of a plain `Boolean`
 */
trait PartialExprFunction[-Ctx, +Expr[+_], -A, +Z] {
	def isDefinedAt(a:A)(implicit ctx: Ctx):Expr[Boolean]
	def apply(a:A)(implicit ctx: Ctx):Z
}

object PartialExprFunction {
	def apply[Ctx, Expr[+_], A, Z](
		isDefinedAtFn:(A, Ctx) => Expr[Boolean],
		applyFn:(A, Ctx) => Z
	):PartialExprFunction[Ctx, Expr, A, Z] = {
		new PartialExprFunction[Ctx, Expr, A, Z] {
			override def isDefinedAt(a:A)(implicit ctx: Ctx):Expr[Boolean] = isDefinedAtFn(a, ctx)
			override def apply(a:A)(implicit ctx: Ctx):Z = applyFn(a, ctx)
		}
	}

	private[stringContextParserCombinator]
	def identity[Ctx, Expr[+_], Type[_], A](
		implicit backing: Exprs[Ctx, Expr, Type]
	):PartialExprFunction[Ctx, Expr, A, A] = {
		new PartialExprFunction[Ctx, Expr, A, A] {
			override def isDefinedAt(a:A)(implicit ctx: Ctx):Expr[Boolean] = backing.constTrue
			override def apply(a:A)(implicit ctx: Ctx):A = a
		}
	}
}
