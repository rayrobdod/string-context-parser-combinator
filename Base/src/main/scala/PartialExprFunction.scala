package name.rayrobdod.stringContextParserCombinator

/**
 * A partial function which is valid according to an `Expr[Boolean]` instead of a plain `Boolean`
 */
trait PartialExprFunction[+Expr[+_], -A, +Z] {
	def isDefinedAt(a:A):Expr[Boolean]
	def apply(a:A):Z
}

object PartialExprFunction {
	def apply[Expr[+_], A, Z](
		isDefinedAtFn:A => Expr[Boolean],
		applyFn:A => Z
	):PartialExprFunction[Expr, A, Z] = {
		new PartialExprFunction[Expr, A, Z] {
			override def isDefinedAt(a:A):Expr[Boolean] = isDefinedAtFn(a)
			override def apply(a:A):Z = applyFn(a)
		}
	}

	private[stringContextParserCombinator]
	def identity[Expr[+_], A](
		constTrue:Expr[Boolean]
	):PartialExprFunction[Expr, A, A] = {
		new PartialExprFunction[Expr, A, A] {
			override def isDefinedAt(a:A):Expr[Boolean] = constTrue
			override def apply(a:A):A = a
		}
	}
}
