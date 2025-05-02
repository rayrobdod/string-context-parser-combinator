package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Map {
	def interpolator[Ctx, Expr, A, Z](
		backing:Interpolator[Ctx, Expr, A],
		mapping: (A, Ctx) => Z,
	):Interpolator[Ctx, Expr, Z] = {
		new Interpolator[Ctx, Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapValues(value => mapping(value, ctx))
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A, Z](
		backing:Extractor[Ctx, Expr, Type, A],
		contramapping: (Z, Ctx) => A,
	):Extractor[Ctx, Expr, Type, Z] = {
		new Extractor[Ctx, Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				backing.extractor(input).mapValues({(x:UnapplyExpr[Ctx, Expr, Type, A]) => exprs.contramap(x, contramapping)})
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A, Z](
		backing:Parser[Ctx, Expr, Type, A],
		mapping: (A, Ctx) => Z,
		contramapping: (Z, Ctx) => A,
	):Parser[Ctx, Expr, Type, Z] = {
		new Parser[Ctx, Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapValues(value => mapping(value, ctx))
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				backing.extractor(input).mapValues({(x:UnapplyExpr[Ctx, Expr, Type, A]) => exprs.contramap(x, contramapping)})
			}
		}
	}
}
