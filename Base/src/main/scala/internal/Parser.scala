package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
trait Interpolator[-Expr, +A] {
	def interpolate[ExprZ <: Expr, Pos](
		input:Input[ExprZ, Pos])(
		implicit ev1:Ordering[Pos]
	):Result[ExprZ, Pos, A]
}

private[stringContextParserCombinator]
trait Extractor[Expr[+_], Type[_], -A] {
	def extractor[Pos](
		input:Input[Unit, Pos])(
		implicit ev1:Ordering[Pos],
		exprs:UnapplyExprs[Expr, Type]
	):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]]
}

private[stringContextParserCombinator]
trait Parser[Expr[+_], Type[_], A]
       extends Interpolator[Expr[Any], A]
       with Extractor[Expr, Type, A]
