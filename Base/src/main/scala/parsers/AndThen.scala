package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class AndThen[U <: Context with Singleton, A, B, Z](
	left:Parser[U, A], right:Parser[U, B], ev:Implicits.AndThenTypes[A, B, Z]
) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right.parse(resa) match {
				case Success(b, resb) => Success(ev.aggregate(a,b), resb)
				case Failure(found, expect) => Failure(found, expect)
			}
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
