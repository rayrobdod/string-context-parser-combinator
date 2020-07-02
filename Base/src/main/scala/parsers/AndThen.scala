package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A], right:Parser[Expr, B], ev:Implicits.AndThenTypes[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(valA, restA, traceA) => right.parse(restA) match {
				case Success(valB, restB, traceB) => Success(ev.aggregate(valA, valB), restB, ThenTrace(traceA, traceB))
				case Failure(traceB) => Failure(ThenTrace(traceA, traceB))
			}
			case failure@Failure(_) => failure
		}
	}
}
