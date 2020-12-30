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
private[stringContextParserCombinator]
final class RepeatAndThen[Expr, A, AS, B, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	delimiter:Parser[Expr, Unit],
	evL:typelevel.Repeated[A, AS],
	rhs:Parser[Expr, B],
	evR:typelevel.Sequenced[AS, B, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		var counter:Int = 0
		val accumulator = evL.init()
		var remaining:Input[Expr] = input
		var continue:Boolean = true
		var repeatFailure:Failure[Expr] = null
		var repeatAnySuccessHasCut:Cut = Cut.False
		val states = scala.collection.mutable.Stack[Success[Expr, AS]]()

		def thenTrace(left:Trace[Expr], right:Trace[Expr]):Trace[Expr] = {
			if (left.isInstanceOf[EmptyTrace[_]])
				{right}
			else if (right.isInstanceOf[EmptyTrace[_]])
				{left}
			else
				{ThenTrace(left, right)}
		}
		def foldOrTrace(left:Trace[Expr], right:Trace[Expr]):Trace[Expr] = (left, right) match {
			case (ThenTrace(leftLeft, leftRight), ThenTrace(rightLeft, rightRight)) if leftLeft == rightLeft => {
				ThenTrace(leftLeft, foldOrTrace(leftRight, rightRight))
			}
			case _ => OrTrace(left, right)
		}

		states.push(Success(evL.result(accumulator), input, EmptyTrace(input), Cut.False))
		while (continue && counter < max) {
			var delimTrace:Trace[Expr] = EmptyTrace(input)
			if (counter != 0) {
				delimiter.parse(remaining) match {
					case Success((), r, t, c) => {
						repeatAnySuccessHasCut = repeatAnySuccessHasCut | c
						remaining = r
						delimTrace = t
					}
					case Failure(t, c) => {
						repeatFailure = Failure(thenTrace(states.head.trace, t), c)
						continue = false
					}
				}
			}
			if (continue) {
				inner.parse(remaining) match {
					case Success(a, r, t, c) => {
						counter += 1
						repeatAnySuccessHasCut = repeatAnySuccessHasCut | c
						evL.append(accumulator, a)
						states.push(Success(evL.result(accumulator), r, thenTrace(thenTrace(states.head.trace, delimTrace), t), c))
						continue = (remaining != r) // quit if inner seems to be making no progress
						remaining = r
					}
					case Failure(t, c) => {
						repeatFailure = Failure(thenTrace(thenTrace(states.head.trace, delimTrace), t), c)
						continue = false
					}
				}
			}
		}

		if (repeatFailure != null && repeatFailure.isCut.toBoolean) {
			return repeatFailure
		}

		var rhsFailure:Failure[Expr] = null
		continue = true
		while (continue && counter >= min && states.nonEmpty) {
			val Success(topValue, topRemaining, topTrace, topCut) = states.pop()
			rhs.parse(topRemaining) match {
				case Success(a, r, t, c) => {
					return Success(evR.aggregate(topValue, a), r, thenTrace(topTrace, t), repeatAnySuccessHasCut | c)
				}
				case Failure(t, c) => {
					if (rhsFailure == null) {
						rhsFailure = Failure(thenTrace(topTrace, t), c)
					}
					continue = ! topCut.toBoolean
					counter = counter - 1
					// try next
				}
			}
		}

		if (null == repeatFailure) {
			// means that input saturates the repeat portion of this aggregate
			Failure(rhsFailure.trace, repeatAnySuccessHasCut)
		} else if (null == rhsFailure) {
			// means that input does not meet minimum requirements the repeat portion of this aggregate
			Failure(repeatFailure.trace, repeatAnySuccessHasCut)
		} else if (rhsFailure.isCut.toBoolean) {
			rhsFailure
		} else {
			Failure(foldOrTrace(repeatFailure.trace, rhsFailure.trace), repeatAnySuccessHasCut | rhsFailure.isCut)
		}
	}

	override def andThen[C, Z2](newParser:Parser[Expr, C])(implicit ev:typelevel.Sequenced[Z,C,Z2]):Parser[Expr, Z2] = {
		new RepeatAndThen[Expr, A, AS, (B, C), Z2](
			this.inner,
			this.min,
			this.max,
			this.delimiter,
			this.evL,
			this.rhs.andThen(newParser)(typelevel.Sequenced.sequencedGenricToPair),
			new typelevel.Sequenced[AS, (B, C), Z2] {
				def aggregate(as:AS, bc:(B, C)):Z2 = ev.aggregate(evR.aggregate(as, bc._1), bc._2)
			}
		)
	}
}
