package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[A : Type](using Quotes) extends AbstractParser[Expr[_], Expr[A]] {
	private val expecting = ExpectingDescription(Type.show[A])

	def parse[Pos](input:Input[Expr[_], Pos]):Result[Expr[_], Pos, Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).collect({case x if x.isExprOf[A] => x.asExprOf[A]}),
			expecting,
		)
	}
}
