package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[A : Type](using Quotes) extends AbstractParser[Expr[_], Expr[A]] {
	def parse(input:Input[Expr[_]]):Result[Expr[_], Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).collect({case x if x.isExprOf[A] => x.asExprOf[A]}),
			Expecting(Type.show[A])
		)
	}
}
