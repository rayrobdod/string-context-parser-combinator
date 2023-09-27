package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class End[Expr[_], Type[_]] extends Parser[Expr, Type, Unit] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		this.parse(())(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Unit]] = {
		this.parse(UnapplyExpr.Empty)(input)
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
