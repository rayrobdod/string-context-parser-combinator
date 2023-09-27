package name.rayrobdod.stringContextParserCombinator
package typeclass

/**
 * Describes how to represent an optional value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.optionally Interpolator.optionally]]
 * @tparam A the optional input type
 * @tparam Z the result container
 */
trait Optionally[-A, +Z] {
	/** Returns a `Z` value representing a missing `A` */
	def none:Z
	/** Returns a `Z` value representing the given `A` */
	def some(elem:A):Z
}

/**
 * Describes how to extract an optional value
 *
 * The parser determines whether the some or none branch is taken.
 * The return values' `Expr[Boolean]` indicates whether the value matches the branch
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.optionally Extractor.optionally]]
 * @tparam A the optional input type
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait ContraOptionally[+Expr[+_], +A, -Z] {
	/** Returns whether the `Z` value represents a missing `A` */
	def contraNone(elem:Z):Expr[Boolean]
	/** Returns a PartialExprFunction that indicates whether the `Z` value represents a present `A` and, if so, that `A` value  */
	def contraSome:PartialExprFunction[Expr, Z, A]
}

/**
 * Describes how to both represent and extract an optional value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.optionally Parser.optionally]]
 * @tparam A the optional input type
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait BiOptionally[Expr[+_], A, Z]
	extends Optionally[A, Z]
	with ContraOptionally[Expr, A, Z]

/**
 * Predefined implicit implementations of Optionally
 * and methods to create new Optionally
  */
object Optionally extends LowPrioOptionally {
	/**
	 * Constructs an `Optionally` from a set of functions corresponding to each of Optionally's methods
	 */
	def apply[A, Z](noneFn: => Z, someFn:A => Z):Optionally[A, Z] = {
		final class Apply extends Optionally[A, Z] {
			def none:Z = noneFn
			def some(elem:A):Z = someFn(elem)
		}
		new Apply
	}

	/**
	 * An Optionally in which a present value is used as-is, and `default` is used if the value is missing
	 *
	 * @example
	 * ```scala
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators
	 * given Optionally[Char, Char] = Optionally.whereDefault('A')
	 * val p = idInterpolators.charIn('0' to '9').optionally()
	 * p.interpolate(StringContext("5"), Nil) // '5': Char
	 * p.interpolate(StringContext(""), Nil) // 'A': Char
	 * ```
	 */
	def whereDefault[A](default: => A):Optionally[A, A] = this.apply[A, A](default, Predef.identity _)

	/**
	 * The `Unit`-handling Optionally;
	 * the result is a `Unit` whether the value is present or not
	 *
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	 * val p1:Interpolator[Unit] = ???
	 *
	 * //}
	 * ((p1:Interpolator[Unit]).optionally()):Interpolator[Unit]
	 * ```
	 */
	implicit def unit:Optionally[Unit, Unit] = this.whereDefault(())
}

private[typeclass] trait LowPrioOptionally {
	/**
	 * The fallback `Optionally`;
	 * wraps a present value in a `Some`, and uses `None` as the missing value
	 *
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	 * class A {}
	 * val p1:Interpolator[A] = ???
	 *
	 * //}
	 * ((p1:Interpolator[A]).optionally()):Interpolator[Option[A]]
	 * ```
	 */
	implicit def toOption[A]:Optionally[A, Option[A]] = Optionally(None, Some.apply _)
}

/**
 * Predefined implicit implementations of ContraOptionally
 * and methods to create new ContraOptionally
  */
object ContraOptionally extends VersionSpecificContraOptionally with LowPrioContraOptionally {
	/**
	 * Constructs an `ContraOptionally` from a set of functions corresponding to each of ContraOptionally's methods
	 */
	def apply[Expr[+_], A, Z](
		contraNoneFn:Z => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Expr, Z, A]
	):ContraOptionally[Expr, A, Z] = {
		final class Apply extends ContraOptionally[Expr, A, Z] {
			override def contraNone(elem:Z):Expr[Boolean] = contraNoneFn(elem)
			override def contraSome:PartialExprFunction[Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	implicit def idUnit:ContraOptionally[Id, Unit, Unit] = BiOptionally.idUnit
}

private[typeclass] trait LowPrioContraOptionally extends VersionSpecificLowPrioContraOptionally {
	implicit def idToOption[A]:ContraOptionally[Id, A, Option[A]] = BiOptionally.idToOption
}

/**
 * Predefined implicit implementations of BiOptionally
 * and methods to create new BiOptionally
  */
object BiOptionally extends VersionSpecificBiOptionally with LowPrioBiOptionally {
	/**
	 * Constructs an `BiOptionally` from a set of functions corresponding to each of BiOptionally's methods
	 */
	def apply[Expr[+_], A, Z](
		noneFn: => Z,
		someFn:A => Z,
		contraNoneFn:Z => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Expr, Z, A]
	):BiOptionally[Expr, A, Z] = {
		final class Apply extends BiOptionally[Expr, A, Z] {
			override def none:Z = noneFn
			override def some(elem:A):Z = someFn(elem)
			override def contraNone(elem:Z):Expr[Boolean] = contraNoneFn(elem)
			override def contraSome:PartialExprFunction[Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	implicit def idUnit:BiOptionally[Id, Unit, Unit] = BiOptionally.apply[Id, Unit, Unit](
		(),
		_ => (),
		_ => true,
		PartialExprFunction.identity[Id, Unit](true)
	)
}

private[typeclass] trait LowPrioBiOptionally extends VersionSpecificLowPrioBiOptionally {
	implicit def idToOption[A]:BiOptionally[Id, A, Option[A]] = BiOptionally.apply[Id, A, Option[A]](
		None,
		Some.apply _,
		_.isEmpty,
		PartialExprFunction[Id, Option[A], A](
			_.nonEmpty,
			_.get
		)
	)
}
