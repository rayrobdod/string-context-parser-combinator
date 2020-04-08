package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Map[U <: Context with Singleton, A, Z](
	backing:Parser[U,A], mapping:Function1[A, Z]
) extends AbstractParser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		backing.parse(input) match {
			case Success(v, r) => Success(mapping(v), r)
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
