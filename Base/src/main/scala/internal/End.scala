package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class End[Ctx, Expr[+_], Type[_]] extends Parser[Ctx, Expr, Type, Unit] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		this.parse(())(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Unit]] = {
		this.parse(exprs.empty)(input)
	}

	private def parse[Value, ExprZ, Pos](successfulValue:Value)(input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Value] = {
		val expecting = Expecting(description, input.position)
		if (input.isEmpty) {
			Success(successfulValue, input, ExpectingSet(expecting))
		} else {
			Failure(ExpectingSet(expecting))
		}
	}

	private def description:ExpectingDescription = ExpectingDescription("EOF")
}
