package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.ClassTag
import com.eed3si9n.ifdef.ifdef

/**
 * Describes how to represent an optional value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.optionally Interpolator.optionally]]
 * @tparam A the optional input type
 * @tparam Z the result container
 */
trait Optionally[-Ctx, -A, +Z] {
	/** Returns a `Z` value representing a missing `A` */
	def none(implicit ctx: Ctx):Z
	/** Returns a `Z` value representing the given `A` */
	def some(elem:A)(implicit ctx: Ctx):Z
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
trait ContraOptionally[-Ctx, +Expr[+_], +A, -Z] {
	/** Returns whether the `Z` value represents a missing `A` */
	def contraNone(elem:Z)(implicit ctx: Ctx):Expr[Boolean]
	/** Returns a PartialExprFunction that indicates whether the `Z` value represents a present `A` and, if so, that `A` value  */
	def contraSome:PartialExprFunction[Ctx, Expr, Z, A]
}

/**
 * Describes how to both represent and extract an optional value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.optionally Parser.optionally]]
 * @tparam A the optional input type
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait BiOptionally[-Ctx, Expr[+_], A, Z]
	extends Optionally[Ctx, A, Z]
	with ContraOptionally[Ctx, Expr, A, Z]

/**
 * Predefined implicit implementations of Optionally
 * and methods to create new Optionally
  */
object Optionally extends LowPrioOptionally {
	/**
	 * Constructs an `Optionally` from a set of functions corresponding to each of Optionally's methods
	 */
	def apply[Ctx, A, Z](noneFn: (Ctx) => Z, someFn: (A, Ctx) => Z):Optionally[Ctx, A, Z] = {
		final class Apply extends Optionally[Ctx, A, Z] {
			def none(implicit ctx: Ctx):Z = noneFn(ctx)
			def some(elem:A)(implicit ctx: Ctx):Z = someFn(elem, ctx)
		}
		new Apply
	}

	/**
	 * An Optionally in which a present value is used as-is, and `default` is used if the value is missing
	 *
	 * @example
	 * ```scala
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators
	 * given Optionally[IdCtx, Char, Char] = Optionally.whereDefault('A')
	 * val p = idInterpolators.charIn('0' to '9').optionally()
	 * p.interpolate(StringContext("5"), Nil) // '5': Char
	 * p.interpolate(StringContext(""), Nil) // 'A': Char
	 * ```
	 */
	def whereDefault[Ctx, A](default: Ctx => A):Optionally[Ctx, A, A] = this.apply[Ctx, A, A](default, (value, _) => value)

	@ifdef("scalaBinaryVersion:3")
	@scala.annotation.targetName("whereDefault2")
	def whereDefault[Ctx, A](default: Ctx ?=> A):Optionally[Ctx, A, A] = this.apply[Ctx, A, A]((ctx) => default(using ctx), (value, _) => value)

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
	implicit def unit:Optionally[Any, Unit, Unit] = this.whereDefault(_ => ())
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
	implicit def toOption[Ctx, A]:Optionally[Ctx, A, Option[A]] = Optionally(_ => None, (value, _) => Some(value))
}

/**
 * Predefined implicit implementations of ContraOptionally
 * and methods to create new ContraOptionally
  */
object ContraOptionally extends LowPrioContraOptionally {
	/**
	 * Constructs an `ContraOptionally` from a set of functions corresponding to each of ContraOptionally's methods
	 */
	def apply[Ctx, Expr[+_], A, Z](
		contraNoneFn:(Z, Ctx) => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Ctx, Expr, Z, A]
	):ContraOptionally[Ctx, Expr, A, Z] = {
		final class Apply extends ContraOptionally[Ctx, Expr, A, Z] {
			override def contraNone(elem:Z)(implicit ctx: Ctx):Expr[Boolean] = contraNoneFn(elem, ctx)
			override def contraSome:PartialExprFunction[Ctx, Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	implicit def idUnit:ContraOptionally[IdCtx, Id, Unit, Unit] = BiOptionally.idUnit

	@ifdef("scalaEpochVersion:2")
	trait ContraOptionallys[Ctx, Expr[+_], Type[_]] extends LowPrioContraOptionallys[Ctx, Expr, Type] {
		implicit def unit:ContraOptionally[Ctx, Expr, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	trait LowPrioContraOptionallys[Ctx, Expr[+_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):ContraOptionally[Ctx, Expr, Expr[A], Expr[Option[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):ContraOptionallys[c.type, c.Expr, c.TypeTag] = {
		new ContraOptionallys[c.type, c.Expr, c.TypeTag] {
			private[this] val backing = BiOptionally.forContext(c)

			override def unit:ContraOptionally[c.type, c.Expr, Unit, Unit] = backing.unit
			override def toExprOption[A](implicit typA:c.TypeTag[A]):ContraOptionally[c.type, c.Expr, c.Expr[A], c.Expr[Option[A]]] = backing.toExprOption[A]
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit:BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit] = BiOptionally.quotedUnit

}

private[typeclass] trait LowPrioContraOptionally {
	implicit def idToOption[A]:ContraOptionally[IdCtx, Id, A, Option[A]] = BiOptionally.idToOption

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit typA: TypeCreator[A]):BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] = BiOptionally.quotedToExprOption
}

/**
 * Predefined implicit implementations of BiOptionally
 * and methods to create new BiOptionally
  */
object BiOptionally extends LowPrioBiOptionally {
	/**
	 * Constructs an `BiOptionally` from a set of functions corresponding to each of BiOptionally's methods
	 */
	def apply[Ctx, Expr[+_], A, Z](
		noneFn: (Ctx) => Z,
		someFn: (A, Ctx) => Z,
		contraNoneFn:(Z, Ctx) => Expr[Boolean],
		contraSomeFn:PartialExprFunction[Ctx, Expr, Z, A]
	):BiOptionally[Ctx, Expr, A, Z] = {
		final class Apply extends BiOptionally[Ctx, Expr, A, Z] {
			override def none(implicit ctx: Ctx):Z = noneFn(ctx)
			override def some(elem:A)(implicit ctx: Ctx):Z = someFn(elem, ctx)
			override def contraNone(elem:Z)(implicit ctx: Ctx):Expr[Boolean] = contraNoneFn(elem, ctx)
			override def contraSome:PartialExprFunction[Ctx, Expr, Z, A] = contraSomeFn
		}
		new Apply
	}

	/**
	 * @since 0.2.0
	 */
	implicit def unit[Ctx, Expr[+_], Type[_]](implicit backing: Exprs[Ctx, Expr, Type]):BiOptionally[Ctx, Expr, Unit, Unit] = {
		BiOptionally.apply[Ctx, Expr, Unit, Unit](
			_ => (),
			(_, _) => (),
			(_, ctx) => backing.constTrue(ctx),
			PartialExprFunction.identity(backing)
		)
	}

	//TODO: @deprecated("0.2.0", "use `BiOptionally.unit[IdCtx, Id]` instead")
	implicit def idUnit:BiOptionally[IdCtx, Id, Unit, Unit] = BiOptionally.apply[IdCtx, Id, Unit, Unit](
		_ => (),
		(_, _) => (),
		(_, _) => true,
		PartialExprFunction.identity[IdCtx, Id, ClassTag, Unit]
	)

	@ifdef("scalaEpochVersion:2")
	trait BiOptionallys[Ctx, Expr[+_], Type[_]] extends LowPrioBiOptionallys[Ctx, Expr, Type] {
		implicit def unit:BiOptionally[Ctx, Expr, Unit, Unit]
	}
	@ifdef("scalaEpochVersion:2")
	trait LowPrioBiOptionallys[Ctx, Expr[+_], Type[_]] {
		implicit def toExprOption[A](implicit typA:Type[A]):BiOptionally[Ctx, Expr, Expr[A], Expr[Option[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):BiOptionallys[c.type, c.Expr, c.TypeTag] = {
		new BiOptionallys[c.type, c.Expr, c.TypeTag] {
			private[this] def select[A, Z](qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
			}
			private[this] def selectTermNames[Z](root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				val rootTree = c.universe.Ident(c.universe.TermName(root))
				val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
				c.Expr[Z](namesTree)
			}

			override def unit:BiOptionally[c.type, c.Expr, Unit, Unit] = BiOptionally.apply(
				_ => (),
				(_, _) => (),
				(_, ctx) => Exprs.forContext[c.type].constTrue(ctx),
				PartialExprFunction.identity[c.type, c.Expr, c.TypeTag, Unit](using Exprs.forContext)
			)

			override def toExprOption[A](implicit typA:c.TypeTag[A]):BiOptionally[c.type, c.Expr, c.Expr[A], c.Expr[Option[A]]] = BiOptionally.apply(
				_ => selectTermNames[Option[A]]("_root_", "scala", "None"),
				(value, _) => {
					val rootTree = c.universe.Ident(c.universe.TermName("_root_"))
					val namesTree = List("scala", "Some", "apply").foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
					c.Expr[Option[A]](c.universe.Apply(namesTree, List(value.tree)))
				},
				(value, _) => select[Option[A], Boolean](value, "isEmpty"),
				PartialExprFunction(
					(value, _) => select[Option[A], Boolean](value, "nonEmpty"),
					(value, _) => select[Option[A], A](value, "get")
				)
			)
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit:BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit] = BiOptionally.apply(
		_ => (),
		(_, _) => (),
		(_, ctx) => {
			implicit val quotes = ctx
			scala.quoted.Expr(true)
		},
		PartialExprFunction.identity(Exprs.forQuoted),
	)
}

private[typeclass] trait LowPrioBiOptionally {
	implicit def idToOption[A]:BiOptionally[IdCtx, Id, A, Option[A]] = BiOptionally.apply[IdCtx, Id, A, Option[A]](
		_ => None,
		(value, _:IdCtx) => Some(value),
		(value, _:IdCtx) => value.isEmpty,
		PartialExprFunction[IdCtx, Id, Option[A], A](
			(value, _:IdCtx) => value.nonEmpty,
			(value, _:IdCtx) => value.get
		)
	)

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit typ: TypeCreator[A]):BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] =
		OptionallyImpl.quotedToExprOption[A]
}
