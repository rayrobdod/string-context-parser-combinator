package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class FlatMap[U <: Context with Singleton, A, Z](
	left:Parser[U, A], right:Function1[A, Parser[U, Z]]
) extends AbstractParser[U, Z] {
	def parse(input:Input[U#Expr[_]]):Result[U#Expr[_], Z] = {
		left.parse(input) match {
			case Success(a, resa) => right(a).parse(resa)
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
