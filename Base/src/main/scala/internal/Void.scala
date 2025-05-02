package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Void {
	def interpolator[Ctx, Expr, A](
		backing:Interpolator[Ctx, Expr, A],
	):Interpolator[Ctx, Expr, Unit] = {
		new Interpolator[Ctx, Expr, Unit] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
				backing.interpolate(input).mapValues({_ => ()})
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A](
		backing:Extractor[Ctx, Expr, Type, A],
	):Extractor[Ctx, Expr, Type, Unit] = {
		new Extractor[Ctx, Expr, Type, Unit] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Unit]] = {
				backing.extractor(input).mapValues({_ => exprs.empty})
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A](
		backing:Parser[Ctx, Expr, Type, A],
	):Parser[Ctx, Expr, Type, Unit] = {
		new Parser[Ctx, Expr, Type, Unit] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
				backing.interpolate(input).mapValues({_ => ()})
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Unit]] = {
				backing.extractor(input).mapValues({_ => exprs.empty})
			}
		}
	}
}
