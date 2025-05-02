package name.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always fails */
private[stringContextParserCombinator]
final class Fail[Ctx, Expr[+_], Type[_]](desc:ExpectingDescription) extends Parser[Ctx, Expr, Type, Nothing] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Nothing] = {
		Failure(ExpectingSet(Expecting(desc, input.position)))
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Unit]] = {
		Failure(ExpectingSet(Expecting(desc, input.position)))
	}
}
