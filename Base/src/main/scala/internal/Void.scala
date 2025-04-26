package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Void {
	def interpolator[Expr, A](
		backing:Interpolator[Expr, A],
	):Interpolator[Expr, Unit] = {
		new Interpolator[Expr, Unit] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
				backing.interpolate(input).mapValues({_ => ()})
			}
		}
	}

	def extractor[Expr[+_], Type[_], A](
		backing:Extractor[Expr, Type, A],
	):Extractor[Expr, Type, Unit] = {
		new Extractor[Expr, Type, Unit] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Unit]] = {
				backing.extractor(input).mapValues({_ => exprs.empty})
			}
		}
	}

	def parser[Expr[+_], Type[_], A](
		backing:Parser[Expr, Type, A],
	):Parser[Expr, Type, Unit] = {
		new Parser[Expr, Type, Unit] {
			def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
				backing.interpolate(input).mapValues({_ => ()})
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Unit]] = {
				backing.extractor(input).mapValues({_ => exprs.empty})
			}
		}
	}
}
