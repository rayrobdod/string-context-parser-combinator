package com.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always succeeds */
private[stringContextParserCombinator]
final class Pass extends Parser[Any, Unit] {
	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		Success((), input, ExpectingSet.empty)
	}
}
