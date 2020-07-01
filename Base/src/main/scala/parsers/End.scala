package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class End[Expr] extends AbstractParser[Expr, Unit] {
	override def parse(input:Input[Expr]):Result[Expr, Unit] = {
		if (input.isEmpty) {
			Success((), input)
		} else {
			Failure(this.expecting, input)
		}
	}
	def expecting:Failure.Expecting = Failure.Leaf("EOF")
}
