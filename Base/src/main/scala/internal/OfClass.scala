package name.rayrobdod.stringContextParserCombinator
package internal

/** Succeeds if the next input element is an `arg` with the given type; captures the value */
private[stringContextParserCombinator]
final class OfClass[A](
	clazz:java.lang.Class[A]
) extends Parser[Id, Class, A] {
	private val expecting = ExpectingDescription(s"OfType(${clazz.getName})")

	def interpolate[ExprZ <: Any, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(clazz.isInstance _).map(clazz.cast _),
			this.expecting
		)
	}

	override def extractor[Pos](input:Input[Unit, Pos])(implicit ev1:Ordering[Pos], exprs:UnapplyExprs[Id, Class]):Result[Unit, Pos, UnapplyExpr[Id, Class, Id[A]]] = {
		input.consume(
			_ => None,
			(_:Unit) => Some(exprs.ofType(clazz)),
			expecting
		)
	}
}
