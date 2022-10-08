package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class End extends Parser[Any, Unit] {
	override def parse[ExprZ <: Any, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Unit] = {
		val expecting = Expecting(description, input.position)
		if (input.isEmpty) {
			Success((), input, Set(expecting), Cut.False)
		} else {
			Failure(Set(expecting), Cut.False)
		}
	}

	private def description:ExpectingDescription = ExpectingDescription("EOF")
}
