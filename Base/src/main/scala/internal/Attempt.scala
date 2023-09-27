package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Attempt {
	def interpolator[Expr, A](
		backing:Interpolator[Expr, A]
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Attempt.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)})(input)
			}
		}
	}

	def extractor[Expr[_], Type[_], A](
		backing:Extractor[Expr, Type, A]
	):Extractor[Expr, Type, A] = {
		new Extractor[Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
				Attempt.parse({(x:Input[Unit, Pos]) => backing.extractor(x)})(input)
			}
		}
	}

	def parser[Expr[_], Type[_], A](
		backing:Parser[Expr, Type, A]
	):Parser[Expr, Type, A] = {
		new Parser[Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Attempt.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)})(input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos]):Result[Unit, Pos, UnapplyExpr[Expr, Type, A]] = {
				Attempt.parse({(x:Input[Unit, Pos]) => backing.extractor(x)})(input)
			}
		}
	}

	private def parse[ExprZ, Pos, Value](
		useBacking:Input[ExprZ, Pos] => Result[ExprZ, Pos, Value])(
		input: Input[ExprZ, Pos]
	):Result[ExprZ, Pos, Value] = {
		useBacking(input) match {
			case success:Success[ExprZ, Pos, Value] => success
			case Failure(_, expecting) => Failure(Option(input.position), expecting)
		}
	}
}
