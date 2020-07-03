package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	ev:Implicits.RepeatTypes[A, Z]
) extends AbstractParser[Expr, Z] {
	require(min >= 0)
	require(max >= 1)
	require(max >= min)

	def parse(input:Input[Expr]):Result[Expr, Z] = {
		var counter:Int = 0
		val accumulator = ev.init()
		var remaining:Input[Expr] = input
		var continue:Boolean = true
		var innerFailureTrace:Trace[Expr] = null
		var innerSuccessTrace:Trace[Expr] = EmptyTrace(input)
		var innerCut:Cut = Cut.False
		var innerFailureCut:Cut = Cut.False

		def thenTrace(left:Trace[Expr], right:Trace[Expr]):Trace[Expr] = {
			if (left.isInstanceOf[EmptyTrace[_]]) {right} else {ThenTrace(left, right)}
		}

		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r, t, c) => {
					counter += 1
					innerCut = innerCut | c
					ev.append(accumulator, a)
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
					innerSuccessTrace = thenTrace(innerSuccessTrace, t)
				}
				case Failure(t, c) => {
					innerFailureCut = c
					innerCut = innerCut | c
					innerFailureTrace = t
					continue = false
				}
			}
		}
		if (min <= counter && counter <= max && !(innerFailureCut.toBoolean)) {
			return Success(ev.result(accumulator), remaining, innerSuccessTrace, innerCut)
		} else {
			return Failure(thenTrace(innerSuccessTrace, innerFailureTrace), innerCut)
		}
	}

	override def andThen[B, Z2](rhs:Parser[Expr, B])(implicit ev:Implicits.AndThenTypes[Z,B,Z2]):Parser[Expr, Z2] = {
		new RepeatAndThen[Expr, A, Z, B, Z2](this.inner, this.min, this.max, this.ev, rhs, ev)
	}
}
