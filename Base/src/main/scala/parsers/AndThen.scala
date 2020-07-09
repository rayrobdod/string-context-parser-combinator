package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers] final class AndThen[Expr, A, B, Z](
	left:Parser[Expr, A], right:Parser[Expr, B], ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(valA, restA, traceA, cutA) => right.parse(restA) match {
				case Success(valB, restB, traceB, cutB) => Success(
					ev.aggregate(valA, valB),
					restB,
					ThenTrace(traceA, traceB),
					cutA | cutB
				)
				case Failure(traceB, cutB) => Failure(ThenTrace(traceA, traceB), cutA | cutB)
			}
			case failure@Failure(_,_) => failure
		}
	}
}
