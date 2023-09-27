package name.rayrobdod.stringContextParserCombinator
package internal

/** Acts like the Interpolator when interpolating, and like the Extractor when extracting */
private[stringContextParserCombinator]
final class Paired[Expr[+_], Type[_], A](
	val interpolator:Interpolator[Expr[Any], A],
	val extractor:Extractor[Expr, Type, A]
) extends Parser[Expr, Type, A] {
	override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		interpolator.interpolate(input)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
		extractor.extractor(input)
	}
}
