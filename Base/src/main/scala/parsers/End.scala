package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class End[Expr] extends AbstractParser[Expr, Unit] {
	override def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, Unit] = {
		val expecting = Expecting(description, input.position)
		if (input.isEmpty) {
			Success((), input, Set(expecting), Cut.False)
		} else {
			Failure(Set(expecting), Cut.False)
		}
	}

	private def description:ExpectingDescription = ExpectingDescription("EOF")
}
