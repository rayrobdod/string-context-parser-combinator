package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class OrElse[U <: Context with Singleton, A](
	left:Parser[U, A],
	right:Parser[U, A]
) extends AbstractParser[U, A] {
	def parse(input:Input[U]):Result[U, A] = {
		left.parse(input) match {
			case Success(v, r) => Success(v, r)
			case Failure(found1, expect1) => right.parse(input) match {
				case Success(v, r) => Success(v, r)
				case Failure(found2, expect2) => {
					if (found1._2 == found2._2) {Failure(found1, Failure.Or(Seq(expect1, expect2)))}
					else if (found1._2 > found2._2) {Failure(found1, expect1)}
					else {Failure(found2, expect2)}
				}
			}
		}
	}
}
