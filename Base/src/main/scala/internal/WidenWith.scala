package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class WidenWith[+Expr[_], +Type[_], +A, -Z](
	backing:Extractor[Expr, Type, A],
	contramapping:PartialExprFunction[Expr, Z, A]
) extends Extractor[Expr, Type, Z] {
	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
		backing.extractor(input)
			.mapValues({ee => UnapplyExpr.WidenWith(ee, contramapping)})
	}
}
