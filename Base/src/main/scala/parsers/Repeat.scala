package com.rayrobdod.stringContextParserCombinator
package parsers

private[parsers]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	ev:Implicits.RepeatTypes[A, Z]
) extends AbstractParser[Expr, Z] {
	def parse(input:Input[Expr]):Result[Expr, Z] = {
		var counter:Int = 0
		val accumulator = ev.init()
		var remaining:Input[Expr] = input
		var continue:Boolean = true
		var innerExpecting:Failure[Expr] = null

		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r) => {
					counter += 1
					ev.append(accumulator, a)
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case Failure(expect, rest) => {
					innerExpecting = Failure(expect, rest)
					continue = false
				}
			}
		}
		if (min <= counter && counter <= max) {
			return Success(ev.result(accumulator), remaining)
		} else {
			return innerExpecting
		}
	}

	override def andThen[B, Z2](rhs:Parser[Expr, B])(implicit ev:Implicits.AndThenTypes[Z,B,Z2]):Parser[Expr, Z2] = {
		new RepeatAndThen[Expr, A, Z, B, Z2](this.inner, this.min, this.max, this.ev, rhs, ev)
	}
}
