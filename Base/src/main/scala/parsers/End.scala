package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class End[Expr] extends AbstractParser[Expr, Unit] {
	override def parse(input:Input[Expr]):Result[Expr, Unit] = {
		val trace = LeafTrace(this.expecting, input)
		if (input.isEmpty) {
			Success((), input, trace, Cut.False)
		} else {
			Failure(trace, Cut.False)
		}
	}
	private def expecting:Expecting = Expecting("EOF")
}
