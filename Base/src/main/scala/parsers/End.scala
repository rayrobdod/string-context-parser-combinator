package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class End[U <: Context with Singleton]
extends AbstractParser[U, Unit] {
	override def parse(input:Input[U]):Result[U, Unit] = {
		if (input.isEmpty) {
			Success((), input)
		} else {
			Failure(this.expecting, input)
		}
	}
	def expecting:Failure.Expecting = Failure.Leaf("EOF")
}
