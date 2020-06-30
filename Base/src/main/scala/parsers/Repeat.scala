package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class Repeat[U <: Context with Singleton, A, Z](
	inner:Parser[U, A],
	min:Int,
	max:Int,
	ev:Implicits.RepeatTypes[A, Z]
) extends AbstractParser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		var counter:Int = 0
		val accumulator = ev.init()
		var remaining:Input[U] = input
		var continue:Boolean = true
		var innerExpecting:Failure[U] = null

		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r) => {
					counter += 1
					ev.append(accumulator, a)
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case failure:Failure[U] => {
					innerExpecting = failure
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

	override def andThen[B, Z2](rhs:Parser[U, B])(implicit ev:Implicits.AndThenTypes[Z,B,Z2]):Parser[U, Z2] = {
		new RepeatAndThen[U, A, Z, B, Z2](this.inner, this.min, this.max, this.ev, rhs, ev)
	}
}
