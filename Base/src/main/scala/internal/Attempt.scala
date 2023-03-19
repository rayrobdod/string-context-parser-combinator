package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
object Attempt {
	def interpolator[Expr, A](
		backing:Interpolator[Expr, A]
	):Interpolator[Expr, A] = {
		new Interpolator[Expr, A] {
			def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
				Attempt.parse({(x:Input[ExprZ, Pos]) => backing.interpolate(x)})(input)
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
