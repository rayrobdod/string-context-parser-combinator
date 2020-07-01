package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class OrElse[U <: Context with Singleton, A](
	left:Parser[U, A],
	right:Parser[U, A]
) extends AbstractParser[U, A] {
	def parse(input:Input[U#Expr[_]]):Result[U#Expr[_], A] = {
		left.parse(input) match {
			case Success(v, r) => Success(v, r)
			case Failure(expect1, remain1) => right.parse(input) match {
				case Success(v, r) => Success(v, r)
				case Failure(expect2, remain2) => {
					Failure(Failure.Leaf("TODO OrElse Error Reporting"), input)
				}
			}
		}
	}
}
