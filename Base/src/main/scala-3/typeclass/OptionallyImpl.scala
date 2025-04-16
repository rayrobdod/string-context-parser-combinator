package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.quoted.*

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[typeclass]
object OptionallyImpl {
	private[typeclass]
	implicit def quotedToExprOption[A](using Quotes, Type[A]):BiOptionally[Expr, Expr[A], Expr[Option[A]]] =
		BiOptionally.apply(
			Expr(None),
			value => '{Some($value)},
			value => '{$value.isEmpty},
			PartialExprFunction(
				value => '{$value.nonEmpty},
				value => '{$value.get},
			),
		)
}
