package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Opaque {
	def interpolator[Expr, A](
		backing:Interpolator[Expr, A],
		description:ExpectingDescription
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Opaque.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)}, description, input)
			}
		}
	}

	def extractor[Expr[_], Type[_], A](
		backing:Extractor[Expr, Type, A],
		description:ExpectingDescription
	):Extractor[Expr, Type, A] = {
		new Extractor[Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
				Opaque.parse({(x:Input[Unit, Pos]) => backing.extractor(x)}, description, input)
			}
		}
	}

	def parser[Expr[_], Type[_], A](
		backing:Parser[Expr, Type, A],
		description:ExpectingDescription
	):Parser[Expr, Type, A] = {
		new Parser[Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Opaque.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)}, description, input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Expr, Type]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
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
