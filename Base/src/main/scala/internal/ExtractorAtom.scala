package name.rayrobdod.stringContextParserCombinator
package internal

/**
 * As a contraparser, this will parse using the backing parser,
 * and create an ExtractorExpr that checks whether the value is equal to the parsed value
 * As a parser, this will pass through the result of the backing parser.
 */
private[stringContextParserCombinator]
final class ExtractorAtom[Ctx, Expr[+_], Type[_], A](
	backing:Interpolator[Ctx, Expr[Any], Expr[A]],
	tpeA:Type[A]
) extends Parser[Ctx, Expr, Type, Expr[A]] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Expr[A]] = {
		backing.interpolate(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Expr[A]]] = {
		input.justCurrentPartConsume[Ctx, Expr[Any], Expr[A]](backing)
			.mapValues(value => exprs.isEqualTo(value)(tpeA))
	}
}
