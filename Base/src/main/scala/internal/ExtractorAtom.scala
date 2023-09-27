package name.rayrobdod.stringContextParserCombinator
package internal

/**
 * As a contraparser, this will parse using the backing parser,
 * and create an ExtractorExpr that checks whether the value is equal to the parsed value
 * As a parser, this will pass through the result of the backing parser.
 */
private[stringContextParserCombinator]
final class ExtractorAtom[Expr[_], Type[_], A](
	backing:Interpolator[Expr[Any], Expr[A]],
	tpeA:Type[A]
) extends Parser[Expr, Type, Expr[A]] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Expr[A]] = {
		backing.interpolate(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Expr[A]]] = {
		input.justCurrentPartConsume[Expr[Any], Expr[A]](backing)
			.mapValues(value => UnapplyExpr.IsEqualTo(value, tpeA))
	}
}
