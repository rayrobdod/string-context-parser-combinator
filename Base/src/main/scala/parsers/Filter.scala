package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Filter[U <: Context with Singleton, A](
	backing:Parser[U, A], predicate:Function1[A, Boolean], description:Failure.Expecting
) extends AbstractParser[U, A] {
	def parse(input:Input[U]):Result[U, A] = {
		backing.parse(input) match {
			case Success(value, remain) if predicate(value) => Success(value, remain)
			case Success(_, _) => Failure(input.next, description)
			case Failure(found, exp) => Failure(found, exp)
		}
	}
}
