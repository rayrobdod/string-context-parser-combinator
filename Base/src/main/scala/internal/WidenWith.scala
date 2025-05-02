package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class WidenWith[Ctx, Expr[+_], Type[_], A, Z](
	backing:Extractor[Ctx, Expr, Type, A],
	contramapping:PartialExprFunction[Ctx, Expr, Z, A]
) extends Extractor[Ctx, Expr, Type, Z] {
	override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
		backing.extractor(input)
			.mapValues({ee => exprs.widenWith(ee, contramapping)})
	}
}
