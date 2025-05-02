package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Hide {
	def interpolator[Ctx, Expr, Z](
		backing:Interpolator[Ctx, Expr, Z]
	):Interpolator[Ctx, Expr, Z] = {
		new Interpolator[Ctx, Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], Z](
		backing:Extractor[Ctx, Expr, Type, Z]
	):Extractor[Ctx, Expr, Type, Z] = {
		new Extractor[Ctx, Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				backing.extractor(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], Z](
		backing:Parser[Ctx, Expr, Type, Z]
	):Parser[Ctx, Expr, Type, Z] = {
		new Parser[Ctx, Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapExpecting(_ => ExpectingSet.empty)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				backing.extractor(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}
}
