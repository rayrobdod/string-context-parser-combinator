package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class End[Expr] extends AbstractParser[Expr, Unit] {
	override def parse(input:Input[Expr]):Result[Expr, Unit] = {
		if (input.isEmpty) {
			Success((), input, Set.empty, Cut.False)
		} else {
			Failure(Expecting(description, input.position), Cut.False)
		}
	}

	private def description:ExpectingDescription = ExpectingDescription("EOF")
}
