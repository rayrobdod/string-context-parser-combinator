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
			case Failure(expect1, remain1) => right.parse(input) match {
				case Success(v, r) => Success(v, r)
				case Failure(expect2, remain2) => {
					if (remain1.next._2 == remain2.next._2) {Failure(Failure.Or(Seq(expect1, expect2)), remain1)}
					else if (remain1.next._2 > remain2.next._2) {Failure(expect1, remain1)}
					else {Failure(expect2, remain2)}
				}
			}
		}
	}
}
