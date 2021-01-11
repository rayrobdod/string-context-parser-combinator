package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class AndThenWithCut[Expr, A, B, Z](
	left:Parser[Expr, A],
	right:Parser[Expr, B],
	ev:typelevel.Sequenced[A, B, Z]
) extends Parser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		left.parse(input) match {
			case Success(valA, restA, traceA, _) => right.parse(restA) match {
				case Success(valB, restB, traceB, _) => Success(ev.aggregate(valA, valB), restB, ThenTrace(traceA, traceB), Cut.True)
				case Failure(traceB, _) => Failure(traceB, Cut.True)
			}
			case failure@Failure(_, _) => failure
		}
	}
}
