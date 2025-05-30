package name.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always succeeds */
private[stringContextParserCombinator]
final class Pass[Ctx, Expr[+_], Type[_]] extends Parser[Ctx, Expr, Type, Unit] {
	override def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		Success((), input, ExpectingSet.empty)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Unit]] = {
		Success(exprs.empty, input, ExpectingSet.empty)
	}
}
