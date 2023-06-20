package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Hide {
	def interpolator[Expr, Z](
		backing:Interpolator[Expr, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}

	def extractor[Expr[_], Type[_], Z](
		backing:Extractor[Expr, Type, Z]
	):Extractor[Expr, Type, Z] = {
		new Extractor[Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				backing.extractor(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}

	def parser[Expr[_], Type[_], Z](
		backing:Parser[Expr, Type, Z]
	):Parser[Expr, Type, Z] = {
		new Parser[Expr, Type, Z] {
			def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				backing.interpolate(input).mapExpecting(_ => ExpectingSet.empty)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				backing.extractor(input).mapExpecting(_ => ExpectingSet.empty)
			}
		}
	}
}
