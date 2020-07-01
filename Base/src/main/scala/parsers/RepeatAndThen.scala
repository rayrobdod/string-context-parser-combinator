package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Seq

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
		var innerExpecting:Failure[Expr] = null
		val states = scala.collection.mutable.Stack[Success[Expr, AS]]()

		states.push(Success(evL.result(accumulator), input))
		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r) => {
					counter += 1
					evL.append(accumulator, a)
					states.push(Success(evL.result(accumulator), r))
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case Failure(expect, rest) => {
					innerExpecting = Failure(expect, rest)
					continue = false
				}
			}
		}

		var rhsExpecting:Failure[Expr] = null
		while (counter >= min && states.nonEmpty) {
			val top = states.pop()
			rhs.parse(top.remaining) match {
				case Success(a, r) => {
					return Success(evR.aggregate(top.value, a), r)
				}
				case Failure(expect, rest) => {
					if (rhsExpecting == null) {
						rhsExpecting = Failure(expect, rest)
					}
					counter = counter - 1
					// try next
				}
			}
		}

		if (null == innerExpecting) {
			// means that input saturates the repeat portion of this aggregate
			rhsExpecting
		} else if (null == rhsExpecting) {
			// means that input does not meet minimum requirements the repeat portion of this aggregate
			innerExpecting
		} else {
			Failure(Failure.Or(Seq(innerExpecting.expecting, rhsExpecting.expecting)), innerExpecting.remaining)
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
