package com.rayrobdod.stringContextParserCombinator
package internal

private[stringContextParserCombinator]
final class End extends Interpolator[Any, Unit] {
	override def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, Unit] = {
		val expecting = Expecting(description, input.position)
		if (input.isEmpty) {
			Success((), input, ExpectingSet(expecting))
		} else {
			Failure(ExpectingSet(expecting))
		}
	}

	private def description:ExpectingDescription = ExpectingDescription("EOF")
}
