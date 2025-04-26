package name.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always succeeds */
private[stringContextParserCombinator]
final class Pass[Expr[+_], Type[_]] extends Parser[Expr, Type, Unit] {
	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		Success((), input, ExpectingSet.empty)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Unit]] = {
		Success(exprs.empty, input, ExpectingSet.empty)
	}
}
