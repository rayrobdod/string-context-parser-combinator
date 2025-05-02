package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.annotation.nowarn
import scala.quoted.*

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[typeclass]
object OptionallyImpl {
	private[typeclass]
	implicit def quotedToExprOption[A](using TypeCreator[A]):BiOptionally[Quotes, Expr, Expr[A], Expr[Option[A]]] =
		BiOptionally.apply(
			(ctx) => {
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				Expr(None)
			},
			(value, ctx) => {
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				'{Some($value)}
			},
			(value, ctx) => {
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				'{$value.isEmpty}
			},
			PartialExprFunction[Quotes, Expr, Expr[Option[A]], Expr[A]](
				(value, ctx) => {
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					'{$value.nonEmpty}
				},
				(value, ctx) => {
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					'{$value.get}
				},
			),
		)
}
