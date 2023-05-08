package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Map {
	def interpolator[Expr, A, Z](
		backing:Interpolator[Expr, A],
		mapping: A => Z
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapValues(mapping)
			}
		}
	}

	def extractor[Expr[_], Type[_], A, Z](
		backing:Extractor[Expr, Type, A],
		contramapping: Z => A
	):Extractor[Expr, Type, Z] = {
		new Extractor[Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				backing.extractor(input).mapValues({(x:UnapplyExpr[Expr, Type, A]) => exprs.contramap(x, contramapping)})
			}
		}
	}

	def parser[Expr[_], Type[_], A, Z](
		backing:Parser[Expr, Type, A],
		mapping: A => Z,
		contramapping: Z => A
	):Parser[Expr, Type, Z] = {
		new Parser[Expr, Type, Z] {
			def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapValues(mapping)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				backing.extractor(input).mapValues({(x:UnapplyExpr[Expr, Type, A]) => exprs.contramap(x, contramapping)})
			}
		}
	}
}
