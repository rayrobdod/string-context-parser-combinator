package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.reflect.macros.blackbox.Context

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[Ctx <: Context with Singleton, A](
	tpetag:Ctx#TypeTag[A]
) extends Parser[Ctx#Expr[_], Ctx#Expr[A]] {
	private val expecting = ExpectingDescription(s"OfType(${tpetag.tpe})")

	def parse[ExprZ <: Ctx#Expr[_], Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, Ctx#Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(x => x.actualType <:< tpetag.tpe).map(_.asInstanceOf[Ctx#Expr[A]]),
			this.expecting
		)
	}
}
