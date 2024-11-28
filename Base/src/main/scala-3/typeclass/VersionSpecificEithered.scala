package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.quoted.*

private[typeclass] trait VersionSpecificEithered extends LowPrioEithered {
	given unitUnit:Eithered[Unit, Unit, Unit] = Eithered.generic
	given unitAny[B, Z](using ev:Optionally[B, Z]):Eithered[Unit, B, Z] = Eithered(_ => ev.none, ev.some _)
	given anyUnit[A, Z](using ev:Optionally[A, Z]):Eithered[A, Unit, Z] = Eithered(ev.some _, _ => ev.none)

	/**
	 * @since 0.1.1
	 */
	def quotedSplicePiece[A]: Eithered[Expr[A], Expr[Iterable[A]], Repeated.SplicePiece[Expr, A]] =
			Eithered(new Repeated.SplicePiece.One(_), new Repeated.SplicePiece.Many(_))
}

private[typeclass] trait LowPrioEithered {
	/**
	 * The fallback Eithered;
	 * creates a union type of the two component types.
	 *
	 * Since the union of a type with itself is equivalent to that same type,
	 * if this Eithered is used for two parsers of the same type,
	 * then the result is a parser of that type.
	 */
	given generic[A, B]:Eithered[A, B, A | B] = Eithered.apply(Predef.identity _, Predef.identity _)
}

private[typeclass] trait VersionSpecificContraEithered extends LowPrioContraEithered {
	given quotedUnitUnit(using Quotes):ContraEithered[Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	given idUnitUnit:ContraEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioContraEithered {
	given quotedSymmetric[A](using Quotes):ContraEithered[Expr, A, A, A] = BiEithered.quotedSymmetric
	given idSymmetric[A]:ContraEithered[Id, A, A, A] = BiEithered.idSymmetric
}

private[typeclass] trait VersionSpecificBiEithered extends LowPrioBiEithered {
	given quotedUnitUnit(using Quotes):BiEithered[Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	given eitherUnitAny[Expr[_], B, Z](using ev:BiOptionally[Expr, B, Z])(using Quotes):BiEithered[Expr, Unit, B, Z] = BiEithered(
		_ => ev.none,
		ev.some _,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
		ev.contraSome,
	)
	given eitherAnyUnit[Expr[_], A, Z](using ev:BiOptionally[Expr, A, Z])(using Quotes):BiEithered[Expr, A, Unit, Z] = BiEithered(
		ev.some _,
		_ => ev.none,
		ev.contraSome,
		PartialExprFunction[Expr, Z, Unit](
			ev.contraNone,
			_ => ()
		),
	)

	given idUnitUnit:BiEithered[Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioBiEithered {
	given quotedSymmetric[A](using Quotes):BiEithered[Expr, A, A, A] = BiEithered.apply(
		Predef.identity _,
		Predef.identity _,
		PartialExprFunction.identity(Expr(true)),
		PartialExprFunction.identity(Expr(true)),
	)

	given idSymmetric[A]:BiEithered[Id, A, A, A] = BiEithered.apply(
		Predef.identity _,
		Predef.identity _,
		PartialExprFunction.identity[Id, A](true),
		PartialExprFunction.identity[Id, A](true),
	)
}
