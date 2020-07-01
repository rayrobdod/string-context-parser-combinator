package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A], right:Parser[Expr, B], ev:Implicits.AndThenTypes[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right.parse(resa) match {
				case Success(b, resb) => Success(ev.aggregate(a,b), resb)
				case Failure(found, expect) => Failure(found, expect)
			}
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
