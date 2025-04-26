package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object AndThen {
	def interpolator[Expr, A, B, Z](
		left:Interpolator[Expr, A],
		right:Interpolator[Expr, B],
		combiner:typeclass.Sequenced[A, B, Z]
	):Interpolator[Expr, Z] = {
		new Interpolator[Expr, Z] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				AndThen.parse(
					{(x:Input[ExprZ, Pos]) => left.interpolate(x)},
					{(x:Input[ExprZ, Pos]) => right.interpolate(x)},
					combiner.aggregate _,
					input
				)
			}
		}
	}

	def extractor[Expr[+_], Type[_], A, B, Z](
		left:Extractor[Expr, Type, A],
		right:Extractor[Expr, Type, B],
		combiner:typeclass.ContraSequenced[A, B, Z]
	):Extractor[Expr, Type, Z] = {
		new Extractor[Expr, Type, Z] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				AndThen.parse(
					{(x:Input[Unit, Pos]) => left.extractor(x)},
					{(x:Input[Unit, Pos]) => right.extractor(x)},
					{(leftVal:UnapplyExpr[Expr, Type, A], rightVal:UnapplyExpr[Expr, Type, B]) =>
						exprs.sequenced(leftVal, rightVal, combiner)
					},
					input
				)
			}
		}
	}

	def parser[Expr[+_], Type[_], A, B, Z](
		left:Parser[Expr, Type, A],
		right:Parser[Expr, Type, B],
		combiner:typeclass.BiSequenced[A, B, Z]
	):Parser[Expr, Type, Z] = {
		new Parser[Expr, Type, Z] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Z] = {
				AndThen.parse(
					{(x:Input[ExprZ, Pos]) => left.interpolate(x)},
					{(x:Input[ExprZ, Pos]) => right.interpolate(x)},
					combiner.aggregate _,
					input
				)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, Z]] = {
				AndThen.parse(
					{(x:Input[Unit, Pos]) => left.extractor(x)},
					{(x:Input[Unit, Pos]) => right.extractor(x)},
					{(leftVal:UnapplyExpr[Expr, Type, A], rightVal:UnapplyExpr[Expr, Type, B]) =>
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
