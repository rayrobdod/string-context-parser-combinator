package name.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always fails */
private[stringContextParserCombinator]
final class Fail[Expr[_], Type[_]](desc:ExpectingDescription) extends Parser[Expr, Type, Nothing] {
	def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Nothing] = {
		Failure(ExpectingSet(Expecting(desc, input.position)))
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Unit]] = {
		Failure(ExpectingSet(Expecting(desc, input.position)))
	}
}
