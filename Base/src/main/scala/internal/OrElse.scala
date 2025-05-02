package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object OrElse {
	def interpolator[Ctx, Expr, A, B, Z](
		left:Interpolator[Ctx, Expr, A],
		right:Interpolator[Ctx, Expr, B],
		combiner:typeclass.Eithered[Ctx, A, B, Z]
	):Interpolator[Ctx, Expr, Z] = {
		new Interpolator[Ctx, Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				OrElse.interpolate(left, right, combiner, input)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A, B, Z](
		left:Extractor[Ctx, Expr, Type, A],
		right:Extractor[Ctx, Expr, Type, B],
		combiner:typeclass.ContraEithered[Ctx, Expr, A, B, Z]
	):Extractor[Ctx, Expr, Type, Z] = {
		new Extractor[Ctx, Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				OrElse.extractor(left, right, combiner, input)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A, B, Z](
		left:Parser[Ctx, Expr, Type, A],
		right:Parser[Ctx, Expr, Type, B],
		combiner:typeclass.BiEithered[Ctx, Expr, A, B, Z]
	):Parser[Ctx, Expr, Type, Z] = {
		new Parser[Ctx, Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				OrElse.interpolate(left, right, combiner, input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				OrElse.extractor(left, right, combiner, input)
			}
		}
	}

	private def interpolate[Ctx, Expr, A, B, Z, Pos](
		left:Interpolator[Ctx, Expr, A],
		right:Interpolator[Ctx, Expr, B],
		combiner:typeclass.Eithered[Ctx, A, B, Z],
		input:Input[Expr, Pos])(
		implicit ctx:Ctx, ev1:Ordering[Pos]
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

	private def extractor[Ctx, Expr[+_], Type[_], A, B, Z, Pos](
		left:Extractor[Ctx, Expr, Type, A],
		right:Extractor[Ctx, Expr, Type, B],
		combiner:typeclass.ContraEithered[Ctx, Expr, A, B, Z],
		input:Input[Unit, Pos])(
		implicit
		ctx:Ctx,
		ev1:Ordering[Pos],
		exprs:UnapplyExprs[Ctx, Expr, Type]
	):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
		val leftResult = left.extractor(input).mapValues(leftValue => exprs.eitheredLeft(leftValue, combiner))
		val rightResult = right.extractor(input).mapValues(rightValue => exprs.eitheredRight(rightValue, combiner))

		(leftResult, rightResult) match {
			case (leftSuccess:Success[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]], rightSuccess:Success[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]]) => leftSuccess ++ rightSuccess
			case (leftSuccess:Success[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]], _:Failure[Pos]) => leftSuccess
			case (_:Failure[Pos], rightSuccess:Success[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]]) => rightSuccess
			case (leftFailure:Failure[Pos], rightFailure:Failure[Pos]) => leftFailure or rightFailure
		}
	}
}
