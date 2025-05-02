package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object AndThen {
	def interpolator[Ctx, Expr, A, B, Z](
		left:Interpolator[Ctx, Expr, A],
		right:Interpolator[Ctx, Expr, B],
		combiner:typeclass.Sequenced[Ctx, A, B, Z]
	):Interpolator[Ctx, Expr, Z] = {
		new Interpolator[Ctx, Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				AndThen.parse(
					{(x:Input[ExprZ, Pos]) => left.interpolate(x)},
					{(x:Input[ExprZ, Pos]) => right.interpolate(x)},
					combiner.aggregate _,
					input
				)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A, B, Z](
		left:Extractor[Ctx, Expr, Type, A],
		right:Extractor[Ctx, Expr, Type, B],
		combiner:typeclass.ContraSequenced[Ctx, A, B, Z]
	):Extractor[Ctx, Expr, Type, Z] = {
		new Extractor[Ctx, Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				AndThen.parse(
					{(x:Input[Unit, Pos]) => left.extractor(x)},
					{(x:Input[Unit, Pos]) => right.extractor(x)},
					{(leftVal:UnapplyExpr[Ctx, Expr, Type, A], rightVal:UnapplyExpr[Ctx, Expr, Type, B]) =>
						exprs.sequenced(leftVal, rightVal, combiner)
					},
					input
				)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A, B, Z](
		left:Parser[Ctx, Expr, Type, A],
		right:Parser[Ctx, Expr, Type, B],
		combiner:typeclass.BiSequenced[Ctx, A, B, Z]
	):Parser[Ctx, Expr, Type, Z] = {
		new Parser[Ctx, Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				AndThen.parse(
					{(x:Input[ExprZ, Pos]) => left.interpolate(x)},
					{(x:Input[ExprZ, Pos]) => right.interpolate(x)},
					combiner.aggregate _,
					input
				)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, Z]] = {
				AndThen.parse(
					{(x:Input[Unit, Pos]) => left.extractor(x)},
					{(x:Input[Unit, Pos]) => right.extractor(x)},
					{(leftVal:UnapplyExpr[Ctx, Expr, Type, A], rightVal:UnapplyExpr[Ctx, Expr, Type, B]) =>
						exprs.sequenced(leftVal, rightVal, combiner)
					},
					input
				)
			}
		}
	}

	private def parse[Expr, A, B, Z, Pos](
		useLeft:Input[Expr, Pos] => Result[Expr, Pos, A],
		useRight:Input[Expr, Pos] => Result[Expr, Pos, B],
		combiner:(A, B) => Z,
		input:Input[Expr, Pos]
	):Result[Expr, Pos, Z] = {
		useLeft(input) match {
			case successA:Success[Expr, Pos, A] => successA.flatMap[Expr, Z]({case Success1(valA, restA, expectingA) => useRight(restA) match {
				case successB:Success[Expr, Pos, B] => successB.map[Expr, Z]({case Success1(valB, restB, expectingB) => Success1(
					combiner(valA, valB),
					restB,
					expectingA ++ expectingB
				)})
				case failureB:Failure[Pos] => failureB or expectingA
			}})
			case failure:Failure[Pos] => failure
		}
	}
}
