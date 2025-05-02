package name.rayrobdod.stringContextParserCombinator
package internal

/** Acts like the Interpolator when interpolating, and like the Extractor when extracting */
private[stringContextParserCombinator]
final class Paired[Ctx, Expr[+_], Type[_], A](
	val interpolator:Interpolator[Ctx, Expr[Any], A],
	val extractor:Extractor[Ctx, Expr, Type, A]
) extends Parser[Ctx, Expr, Type, A] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx: Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		interpolator.interpolate(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
		extractor.extractor(input)
	}
}
