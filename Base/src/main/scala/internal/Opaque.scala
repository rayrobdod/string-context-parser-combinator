package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Opaque {
	def interpolator[Ctx, Expr, A](
		backing:Interpolator[Ctx, Expr, A],
		description:ExpectingDescription
	):Interpolator[Ctx, Expr, A] = {
		new Interpolator[Ctx, Expr, A] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Opaque.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)}, description, input)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A](
		backing:Extractor[Ctx, Expr, Type, A],
		description:ExpectingDescription
	):Extractor[Ctx, Expr, Type, A] = {
		new Extractor[Ctx, Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
				Opaque.parse({(x:Input[Unit, Pos]) => backing.extractor(x)}, description, input)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A](
		backing:Parser[Ctx, Expr, Type, A],
		description:ExpectingDescription
	):Parser[Ctx, Expr, Type, A] = {
		new Parser[Ctx, Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Opaque.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)}, description, input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
				Opaque.parse({(x:Input[Unit, Pos]) => backing.extractor(x)}, description, input)
			}
		}
	}

	private def parse[ExprZ, Pos, Value](
		useBacking:Input[ExprZ, Pos] => Result[ExprZ, Pos, Value],
		description:ExpectingDescription,
		input: Input[ExprZ, Pos])(
		implicit ev1:Ordering[Pos]
	):Result[ExprZ, Pos, Value] = {
		val descriptionPosition = ExpectingSet(Expecting(description, input.position))
		useBacking(input) match {
			case success:Success[ExprZ, Pos, Value] => success.map({case Success1(value, rest, _) => Success1(value, rest, descriptionPosition)})
			case _:Failure[Pos] => Failure(descriptionPosition)
		}
	}
}
