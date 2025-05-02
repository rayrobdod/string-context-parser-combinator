package name.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Attempt {
	def interpolator[Ctx, Expr, A](
		backing:Interpolator[Ctx, Expr, A]
	):Interpolator[Ctx, Expr, A] = {
		new Interpolator[Ctx, Expr, A] {
			override def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Attempt.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)})(input)
			}
		}
	}

	def extractor[Ctx, Expr[+_], Type[_], A](
		backing:Extractor[Ctx, Expr, Type, A]
	):Extractor[Ctx, Expr, Type, A] = {
		new Extractor[Ctx, Expr, Type, A] {
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
				Attempt.parse({(x:Input[Unit, Pos]) => backing.extractor(x)})(input)
			}
		}
	}

	def parser[Ctx, Expr[+_], Type[_], A](
		backing:Parser[Ctx, Expr, Type, A]
	):Parser[Ctx, Expr, Type, A] = {
		new Parser[Ctx, Expr, Type, A] {
			override def interpolate[ExprZ <: Expr[Any], Pos](input:Input[ExprZ, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Attempt.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)})(input)
			}
			override def extractor[Pos](input:Input[Unit, Pos])(implicit ctx:Ctx, ev1:Ordering[Pos], exprs:UnapplyExprs[Ctx, Expr, Type]):Result[Unit, Pos, UnapplyExpr[Ctx, Expr, Type, A]] = {
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
