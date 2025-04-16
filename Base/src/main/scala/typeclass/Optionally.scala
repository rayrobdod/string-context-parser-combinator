package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef

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
trait ContraOptionally[+Expr[_], +A, -Z] {
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
trait BiOptionally[Expr[_], A, Z]
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
object ContraOptionally extends LowPrioContraOptionally {
	/**
	 * Constructs an `ContraOptionally` from a set of functions corresponding to each of ContraOptionally's methods
	 */
	def apply[Expr[_], A, Z](
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

	@ifdef("scalaEpochVersion:2")
	trait ContraOptionallys[Expr[_], Type[_]] extends LowPrioContraOptionallys[Expr, Type] {
		implicit def unit:ContraOptionally[Expr, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	trait LowPrioContraOptionallys[Expr[_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):ContraOptionally[Expr, Expr[A], Expr[Option[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):ContraOptionallys[c.Expr, c.TypeTag] = {
		new ContraOptionallys[c.Expr, c.TypeTag] {
			private[this] val backing = BiOptionally.forContext(c)

			override def unit:ContraOptionally[c.Expr, Unit, Unit] = backing.unit
			override def toExprOption[A](implicit typA:c.TypeTag[A]):ContraOptionally[c.Expr, c.Expr[A], c.Expr[Option[A]]] = backing.toExprOption[A]
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit(implicit quotes: scala.quoted.Quotes):BiOptionally[scala.quoted.Expr, Unit, Unit] = BiOptionally.quotedUnit

}

private[typeclass] trait LowPrioContraOptionally {
	implicit def idToOption[A]:ContraOptionally[Id, A, Option[A]] = BiOptionally.idToOption

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit quotes: scala.quoted.Quotes, typA: scala.quoted.Type[A]):BiOptionally[scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] = BiOptionally.quotedToExprOption
}

/**
 * Predefined implicit implementations of BiOptionally
 * and methods to create new BiOptionally
  */
object BiOptionally extends LowPrioBiOptionally {
	/**
	 * Constructs an `BiOptionally` from a set of functions corresponding to each of BiOptionally's methods
	 */
	def apply[Expr[_], A, Z](
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

	@ifdef("scalaEpochVersion:2")
	trait BiOptionallys[Expr[_], Type[_]] extends LowPrioBiOptionallys[Expr, Type] {
		implicit def unit:BiOptionally[Expr, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	trait LowPrioBiOptionallys[Expr[_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):BiOptionally[Expr, Expr[A], Expr[Option[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):BiOptionallys[c.Expr, c.TypeTag] = {
		new BiOptionallys[c.Expr, c.TypeTag] {
			private[this] val exprTrue = c.Expr[Boolean](c.universe.Literal(c.universe.Constant(true)))
			private[this] def select[A, Z](qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
			}
			private[this] def selectTermNames[Z](root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				val rootTree = c.universe.Ident(c.universe.TermName(root))
				val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
				c.Expr[Z](namesTree)
			}

			override def unit:BiOptionally[c.Expr, Unit, Unit] = BiOptionally.apply(
				(),
				_ => (),
				_ => exprTrue,
				PartialExprFunction.identity(exprTrue)
			)

			override def toExprOption[A](implicit typA:c.TypeTag[A]):BiOptionally[c.Expr, c.Expr[A], c.Expr[Option[A]]] = BiOptionally.apply(
				selectTermNames[Option[A]]("_root_", "scala", "None"),
				value => {
					val rootTree = c.universe.Ident(c.universe.TermName("_root_"))
					val namesTree = List("scala", "Some", "apply").foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
					c.Expr[Option[A]](c.universe.Apply(namesTree, List(value.tree)))
				},
				value => select[Option[A], Boolean](value, "isEmpty"),
				PartialExprFunction(
					value => select[Option[A], Boolean](value, "nonEmpty"),
					value => select[Option[A], A](value, "get")
				)
			)
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit(implicit quotes: scala.quoted.Quotes):BiOptionally[scala.quoted.Expr, Unit, Unit] = BiOptionally.apply(
		(),
		_ => (),
		_ => scala.quoted.Expr(true),
		PartialExprFunction.identity(scala.quoted.Expr(true)),
	)
}

private[typeclass] trait LowPrioBiOptionally {
	implicit def idToOption[A]:BiOptionally[Id, A, Option[A]] = BiOptionally.apply[Id, A, Option[A]](
		None,
		Some.apply _,
		_.isEmpty,
		PartialExprFunction[Id, Option[A], A](
			_.nonEmpty,
			_.get
		)
	)

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit quotes: scala.quoted.Quotes, typ: scala.quoted.Type[A]):BiOptionally[scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] =
		OptionallyImpl.quotedToExprOption[A]
}
