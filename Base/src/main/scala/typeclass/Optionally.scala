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
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
  */
object Optionally extends LowPrioOptionally {
	/**
	 * Constructs an `Optionally` from a set of functions corresponding to each of Optionally's methods
	 * @group Support
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
	 * @group AnyContext
	 */
	def whereDefault[Ctx, A](default: Ctx => A):Optionally[Ctx, A, A] = this.apply[Ctx, A, A](default, (value, _) => value)

	/**
	 * @group AnyContext
	 */
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
	 * @group AnyContext
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
	 * @group AnyContext
	 */
	implicit def toOption[A]:Optionally[Any, A, Option[A]] = Optionally(_ => None, (value, _) => Some(value))
}

/**
 * Predefined implicit implementations of ContraOptionally
 * and methods to create new ContraOptionally
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
  */
object ContraOptionally extends LowPrioContraOptionally {
	/**
	 * Constructs an `ContraOptionally` from a set of functions corresponding to each of ContraOptionally's methods
	 * @group Support
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

	/**
	 * @group IdContext
	 */
	implicit def idUnit:ContraOptionally[IdCtx, Id, Unit, Unit] = BiOptionally.idUnit

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:BiOptionally[Ctx, Ctx#Expr, Unit, Unit] = BiOptionally.contextUnit

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit:BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit] = BiOptionally.quotedUnit

}

private[typeclass] trait LowPrioContraOptionally {
	/**
	 * @group IdContext
	 */
	implicit def idToOption[A]:ContraOptionally[IdCtx, Id, A, Option[A]] = BiOptionally.idToOption

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextToExprOption[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A](implicit typA:Ctx#TypeTag[A]):BiOptionally[Ctx, Ctx#Expr, Ctx#Expr[A], Ctx#Expr[Option[A]]] = BiOptionally.contextToExprOption

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit typA: TypeCreator[A]):BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] = BiOptionally.quotedToExprOption
}

/**
 * Predefined implicit implementations of BiOptionally
 * and methods to create new BiOptionally
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
  */
object BiOptionally extends LowPrioBiOptionally {
	/**
	 * Constructs an `BiOptionally` from a set of functions corresponding to each of BiOptionally's methods
	 * @group Support
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
	 * @group AnyContext
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

	/**
	 * @group IdContext
	 */
	//TODO: @deprecated("0.2.0", "use `BiOptionally.unit[IdCtx, Id]` instead")
	implicit def idUnit:BiOptionally[IdCtx, Id, Unit, Unit] = BiOptionally.apply[IdCtx, Id, Unit, Unit](
		_ => (),
		(_, _) => (),
		(_, _) => true,
		PartialExprFunction.identity[IdCtx, Id, ClassTag, Unit]
	)

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	def contextUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:BiOptionally[Ctx, Ctx#Expr, Unit, Unit] = BiOptionally.apply(
		_ => (),
		(_, _) => (),
		(_, ctx) => Exprs.forContext[Ctx].constTrue(ctx),
		PartialExprFunction.identity[Ctx, Ctx#Expr, Ctx#TypeTag, Unit](using Exprs.forContext)
	)

	/**
	 * @group QuotedContext
	 */
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
	/**
	 * @group IdContext
	 */
	implicit def idToOption[A]:BiOptionally[IdCtx, Id, A, Option[A]] = BiOptionally.apply[IdCtx, Id, A, Option[A]](
		_ => None,
		(value, _:IdCtx) => Some(value),
		(value, _:IdCtx) => value.isEmpty,
		PartialExprFunction[IdCtx, Id, Option[A], A](
			(value, _:IdCtx) => value.nonEmpty,
			(value, _:IdCtx) => value.get
		)
	)

	@ifdef("scalaEpochVersion:2")
	private[this] def select[A, Z](c:scala.reflect.macros.blackbox.Context)(qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
		c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
	}
	@ifdef("scalaEpochVersion:2")
	private[this] def selectTermNames[Z](c:scala.reflect.macros.blackbox.Context)(root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
		val rootTree = c.universe.Ident(c.universe.TermName(root))
		val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
		c.Expr[Z](namesTree)
	}

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	def contextToExprOption[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A](implicit typA:Ctx#TypeTag[A]):BiOptionally[Ctx, Ctx#Expr, Ctx#Expr[A], Ctx#Expr[Option[A]]] = BiOptionally.apply(
		(ctx:Ctx) => {
			val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
			import myBindSingletonContexts._
			@annotation.nowarn("msg=never used") implicit val typA2:ctx.TypeTag[A] = typA
			selectTermNames[Option[A]](ctx)("_root_", "scala", "None"): Ctx#Expr[Option[A]]
		},
		(value, ctx) => {
			val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
			import myBindSingletonContexts._
			@annotation.nowarn("msg=never used") implicit val typA2:ctx.TypeTag[A] = typA
			val value2: ctx.Expr[A] = value
			val rootTree = ctx.universe.Ident(ctx.universe.TermName("_root_"))
			val namesTree = List("scala", "Some", "apply").foldLeft[ctx.universe.Tree](rootTree)({(folding, name) => ctx.universe.Select(folding, ctx.universe.TermName(name))})
			ctx.Expr[Option[A]](ctx.universe.Apply(namesTree, List(value2.tree))): Ctx#Expr[Option[A]]
		},
		(value, ctx) => {
			val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
			import myBindSingletonContexts._
			val value2 = value: ctx.Expr[Option[A]]
			select[Option[A], Boolean](ctx)(value2, "isEmpty"): Ctx#Expr[Boolean]
		},
		PartialExprFunction(
			(value, ctx) => {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._
				val value2 = value: ctx.Expr[Option[A]]
				select[Option[A], Boolean](ctx)(value2, "nonEmpty"): Ctx#Expr[Boolean]
			},
			(value, ctx) => {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._
				val value2 = value: ctx.Expr[Option[A]]
				select[Option[A], A](ctx)(value2, "get"): Ctx#Expr[A]
			},
		)
	)

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprOption[A](implicit typ: TypeCreator[A]):BiOptionally[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[Option[A]]] =
		OptionallyImpl.quotedToExprOption[A]
}
