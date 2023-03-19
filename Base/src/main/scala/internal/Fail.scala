package com.rayrobdod.stringContextParserCombinator
package internal

/** A parser that consumes no input and always fails */
private[stringContextParserCombinator]
final class Fail(desc:ExpectingDescription) extends Interpolator[Any, Nothing] {
	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Nothing] = {
		Failure(ExpectingSet(Expecting(desc, input.position)))
	}
}
