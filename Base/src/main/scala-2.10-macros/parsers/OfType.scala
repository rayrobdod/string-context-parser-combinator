package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.reflect.macros.Context

/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
private[stringContextParserCombinator]
final class OfType[Ctx <: Context with Singleton, A](
	tpetag:Ctx#TypeTag[A]
) extends AbstractParser[Ctx#Expr[_], Ctx#Expr[A]] {
	def parse(input:Input[Ctx#Expr[_]]):Result[Ctx#Expr[_], Ctx#Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(x => x.actualType <:< tpetag.tpe).map(_.asInstanceOf[Ctx#Expr[A]]),
			Expecting(s"OfType(${tpetag.tpe})")
		)
	}
}
