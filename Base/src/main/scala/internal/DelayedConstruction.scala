package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object DelayedConstruction {
	def interpolator[Ctx, Expr, A](
		backing:() => name.rayrobdod.stringContextParserCombinator.Interpolator[Ctx, Expr, A]
	):Interpolator[Ctx, Expr, A] = {
		new Interpolator[Ctx, Expr, A] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.apply().impl.interpolate(input)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A](
		backing:() => Extractor[Ctx, Expr, Type, A]
	):Extractor[Ctx, Expr, Type, A] = {
		new Extractor[Ctx, Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
				backing.apply().extractor(input)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A](
		backing:() => Parser[Ctx, Expr, Type, A]
	):Parser[Ctx, Expr, Type, A] = {
		new Parser[Ctx, Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				backing.apply().interpolate(input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
				backing.apply().extractor(input)
			}
		}
	}
}
