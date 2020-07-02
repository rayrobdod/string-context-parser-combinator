package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class End[Expr] extends AbstractParser[Expr, Unit] {
	override def parse(input:Input[Expr]):Result[Expr, Unit] = {
		val trace = LeafTrace(this.expecting, input)
		if (input.isEmpty) {
			Success((), input, trace)
		} else {
			Failure(trace)
		}
	}
	private def expecting:Expecting = Expecting("EOF")
}
