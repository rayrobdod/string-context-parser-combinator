package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
trait Interpolator[-Ctx, -Expr, +A] {
	def interpolate[ExprZ <: Expr, Pos](
		input:Input[ExprZ, Pos])(
		implicit ctx:Ctx, ev1:Ordering[Pos]
	):Result[ExprZ, Pos, A]
}

private[stringContextParserCombinator]
trait Extractor[Ctx, Expr[+_], Type[_], -A] {
	def extractor[Pos](
		input:Input[Unit, Pos])(
		implicit ctx:Ctx,
		ev1:Ordering[Pos],
		exprs:UnapplyExprs[Ctx, Expr, Type]
	):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]]
}

private[stringContextParserCombinator]
trait Parser[Ctx, Expr[+_], Type[_], A]
       extends Interpolator[Ctx, Expr[Any], A]
       with Extractor[Ctx, Expr, Type, A]
