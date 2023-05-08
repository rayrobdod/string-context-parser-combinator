package com.rayrobdod.stringContextParserCombinator
package typeclass

import scala.quoted.*

private[typeclass]
trait VersionSpecificContraOptionally {
	implicit def quotedUnit(using Quotes):BiOptionally[Expr, Unit, Unit] = BiOptionally.quotedUnit
}

private[typeclass]
trait VersionSpecificLowPrioContraOptionally {
	implicit def quotedToExprOption[A](using Quotes, Type[A]):BiOptionally[Expr, Expr[A], Expr[Option[A]]] = BiOptionally.quotedToExprOption
}

private[typeclass]
trait VersionSpecificBiOptionally {
	implicit def quotedUnit(using Quotes):BiOptionally[Expr, Unit, Unit] = BiOptionally.apply(
		(),
		_ => (),
		_ => Expr(true),
		PartialExprFunction.identity(Expr(true)),
	)
}

private[typeclass]
trait VersionSpecificLowPrioBiOptionally {
	implicit def quotedToExprOption[A](using Quotes, Type[A]):BiOptionally[Expr, Expr[A], Expr[Option[A]]] = BiOptionally.apply(
		Expr(None),
		value => '{Some($value)},
		value => '{$value.isEmpty},
		PartialExprFunction(
			value => '{$value.nonEmpty},
			value => '{$value.get},
		),
	)
}
