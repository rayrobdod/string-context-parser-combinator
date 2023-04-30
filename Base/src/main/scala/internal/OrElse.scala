package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object OrElse {
	def interpolator[Expr, A, B, Z](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Eithered[A, B, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				OrElse.interpolate(left, right, combiner, input)
			}
		}
	}

	def extractor[Expr[_], Type[_], A, B, Z](
		left:Extractor[Expr, Type, A],
		right:Extractor[Expr, Type, B],
		combiner:typeclass.ContraEithered[Expr, A, B, Z]
	):Extractor[Expr, Type, Z] = {
		new Extractor[Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				OrElse.extractor(left, right, combiner, input)
			}
		}
	}

	def parser[Expr[_], Type[_], A, B, Z](
		left:Parser[Expr, Type, A],
		right:Parser[Expr, Type, B],
		combiner:typeclass.BiEithered[Expr, A, B, Z]
	):Parser[Expr, Type, Z] = {
		new Parser[Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				OrElse.interpolate(left, right, combiner, input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				OrElse.extractor(left, right, combiner, input)
			}
		}
	}

	private def interpolate[Expr, A, B, Z, Pos](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Eithered[A, B, Z],
		input:Input[Expr, Pos])(
		implicit ev1:Ordering[Pos]
	):Result[Expr, Pos, Z] = {
		left.interpolate(input) match {
			case leftSuccess:Success[Expr, Pos, A] => leftSuccess.mapValues(combiner.left _)
			case leftFailure:Failure[Pos] => {
				if (leftFailure.isPositionGt(input.position)) {
					// consumed input; don't try the other branch
					leftFailure
				} else {
					// assume that ExpectingSet.Empty means no input consumed
					right.interpolate(input) match {
						case rightSuccess:Success[Expr, Pos, B] => rightSuccess.mapValues(combiner.right _)
						case rightFailure:Failure[Pos] => leftFailure or rightFailure
					}
				}
			}
		}
	}

	private def extractor[Expr[_], Type[_], A, B, Z, Pos](
		left:Extractor[Expr, Type, A],
		right:Extractor[Expr, Type, B],
		combiner:typeclass.ContraEithered[Expr, A, B, Z],
		input:Input[Unit, Pos])(
		implicit ev1:Ordering[Pos],
		exprs:UnapplyExprs[Expr, Type]
	):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
		val leftResult = left.extractor(input).mapValues(leftValue => exprs.eitheredLeft(leftValue, combiner))
		val rightResult = right.extractor(input).mapValues(rightValue => exprs.eitheredRight(rightValue, combiner))

		(leftResult, rightResult) match {
			case (leftSuccess:Success[Unit, Pos, UnapplyExpr[Expr, Type, Z]], rightSuccess:Success[Unit, Pos, UnapplyExpr[Expr, Type, Z]]) => leftSuccess ++ rightSuccess
			case (leftSuccess:Success[Unit, Pos, UnapplyExpr[Expr, Type, Z]], _:Failure[Pos]) => leftSuccess
			case (_:Failure[Pos], rightSuccess:Success[Unit, Pos, UnapplyExpr[Expr, Type, Z]]) => rightSuccess
			case (leftFailure:Failure[Pos], rightFailure:Failure[Pos]) => leftFailure or rightFailure
		}
	}
}
