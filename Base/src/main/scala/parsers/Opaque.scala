package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Opaque[U <: Context with Singleton, A](
	backing:Parser[U, A], description:Failure.Expecting
) extends AbstractParser[U, A] {
	def parse(input:Input[U]):Result[U, A] = {
		backing.parse(input) match {
			case Success(v, r) => Success(v,r)
			case Failure(_, rest) => Failure(description, rest)
		}
	}
}
