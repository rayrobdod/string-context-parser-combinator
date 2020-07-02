package com.rayrobdod.stringContextParserCombinator
package parsers

/**
 * `Repeat(inner, min, max, evL).andThen(rhs)(evR)`
 *
 * Required since Repeat is greedy, and AndThen doesn't know how to make a Repeat backtrack.
 *
 * e.x. `"1".repeat().andThen("1")` would fail to match "11", since the repeat would match the entire
 * string, leaving nothing for the andThen, which will not match an EOF and the entire expression fails.
 * With this, after failing to match when the repeat sucks up everything, this will try again with the
 * repeat accepting one less "1" than before, which then allows the rest of the parser to succeed
 */
private[parsers]
final class RepeatAndThen[Expr, A, AS, B, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	evL:Implicits.RepeatTypes[A, AS],
	rhs:Parser[Expr, B],
	evR:Implicits.AndThenTypes[AS, B, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		var counter:Int = 0
		val accumulator = evL.init()
		var remaining:Input[Expr] = input
		var continue:Boolean = true
		var innerFailureTrace:Trace[Expr] = null
		val states = scala.collection.mutable.Stack[Success[Expr, AS]]()

		def thenTrace(left:Trace[Expr], right:Trace[Expr]):Trace[Expr] = {
			if (left.isInstanceOf[EmptyTrace[_]]) {right} else {ThenTrace(left, right)}
		}
		def foldOrTrace(left:Trace[Expr], right:Trace[Expr]):Trace[Expr] = (left, right) match {
			case (ThenTrace(leftLeft, leftRight), ThenTrace(rightLeft, rightRight)) if leftLeft == rightLeft => {
				ThenTrace(leftLeft, foldOrTrace(leftRight, rightRight))
			}
			case _ => OrTrace(left, right)
		}

		states.push(Success(evL.result(accumulator), input, EmptyTrace(input)))
		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r, t) => {
					counter += 1
					evL.append(accumulator, a)
					states.push(Success(evL.result(accumulator), r, thenTrace(states.head.trace, t)))
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case Failure(t) => {
					innerFailureTrace = thenTrace(states.head.trace, t)
					continue = false
				}
			}
		}

		var rhsFailureTrace:Trace[Expr] = null
		while (counter >= min && states.nonEmpty) {
			val Success(topValue, topRemaining, topTrace) = states.pop()
			rhs.parse(topRemaining) match {
				case Success(a, r, t) => {
					return Success(evR.aggregate(topValue, a), r, thenTrace(topTrace, t))
				}
				case Failure(t) => {
					if (rhsFailureTrace == null) {
						rhsFailureTrace = thenTrace(topTrace, t)
					}
					counter = counter - 1
					// try next
				}
			}
		}

		if (null == innerFailureTrace) {
			// means that input saturates the repeat portion of this aggregate
			Failure(rhsFailureTrace)
		} else if (null == rhsFailureTrace) {
			// means that input does not meet minimum requirements the repeat portion of this aggregate
			Failure(innerFailureTrace)
		} else {
			Failure(foldOrTrace(innerFailureTrace, rhsFailureTrace))
		}
	}

	override def andThen[C, Z2](newParser:Parser[Expr, C])(implicit ev:Implicits.AndThenTypes[Z,C,Z2]):Parser[Expr, Z2] = {
		new RepeatAndThen[Expr, A, AS, (B, C), Z2](
			this.inner,
			this.min,
			this.max,
			this.evL,
			this.rhs.andThen(newParser)(Implicits.AndThenTypes.andThenGeneric),
			new Implicits.AndThenTypes[AS, (B, C), Z2] {
				def aggregate(as:AS, bc:(B, C)):Z2 = ev.aggregate(evR.aggregate(as, bc._1), bc._2)
			}
		)
	}
}
