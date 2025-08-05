package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef
import scala.collection.mutable.Builder

/**
 * Describes how to combine a homogeneous sequence of zero-or-more values.
 *
 * When a Repeated is used:
 *  - first, `init` to create an initial value for the accumulator
 *  - then, `append` is called once for each component item in order, using the accumulator and the component item as parameters and returning the next accumulator value
 *  - lastly, `result` is called with the final accumulator value, and the result of this call is overall result.
 *
 * `init` will be called anew on each use, so it is possible to use a mutable accumulator
 * by creating a new builder in the `init` method
 * and returning the `acc` parameter in the append method.
 *
 * Below is an example of implementing and using a custom `Repeated`:
 * ```scala
 * import name.rayrobdod.stringContextParserCombinator.IdCtx
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Repeated
 *
 * // define the marker types
 * case class Digit(value:Int)
 * case class Digits(value:Int)
 *
 * // define the given instance
 * given Repeated[IdCtx, Digit, Digits] with {
 *  type Acc = Int
 *  def init()(implicit ctx:IdCtx):Acc = 0
 *  def append(acc:Acc, elem:Digit)(implicit ctx:IdCtx):Acc = (acc * 10) + elem.value
 *  def result(acc:Acc)(implicit ctx:IdCtx):Digits = new Digits(acc)
 * }
 *
 * // create the parsers
 * val digit:idInterpolators.Interpolator[Digit] = idInterpolators.charIn('0' to '9').map(x => Digit(x - '0'))
 * val digits:idInterpolators.Interpolator[Digits] = digit.repeat(1)// using Repeated[IdCtx, Digit, Digits]
 *
 * // use the parser
 * digits.interpolate(StringContext("1234"), Nil) // Digits(1234): Digits
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.repeat Interpolator.repeat]]
 * @tparam A the repeated input elements
 * @tparam Z the result container
 */
trait Repeated[-Ctx, -A, +Z] {
	/** The accumulator */
	type Acc
	/** Returns a new empty accumulator */
	def init()(implicit ctx:Ctx):Acc
	/** Inserts `elem` into `acc` */
	def append(acc:Acc, elem:A)(implicit ctx:Ctx):Acc
	/** Transforms `acc` into a Z */
	def result(acc:Acc)(implicit ctx:Ctx):Z
}

/**
 * Describes how to break apart a homogeneous sequence of zero-or-more values into its component parts.
 *
 * The parser determines how many parts a value has
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.repeat Extractor.repeat]]
 * @tparam A the repeated input elements
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait ContraRepeated[-Ctx, +Expr[+_], +A, Z] {
	def headTail:PartialExprFunction[Ctx, Expr, Z, (A, Z)]
	def isEmpty(it:Z)(implicit ctx:Ctx):Expr[Boolean]
}

/**
 * Describes how to combine and break apart a repeated value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.repeat Parser.repeat]]
 * @tparam A the repeated input elements
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait BiRepeated[-Ctx, Expr[+_], A, Z]
	extends Repeated[Ctx, A, Z]
	with ContraRepeated[Ctx, Expr, A, Z]

/**
 * Predefined implicit implementations of Repeated
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
object Repeated extends LowPrioRepeated {
	/**
	 * @group Support
	 */
	private[typeclass] def apply[Ctx, A, Acc, Z](
		initFn: (Ctx) => Acc,
		appendFn: (Acc, A, Ctx) => Acc,
		resultFn: (Acc, Ctx) => Z,
	): Repeated[Ctx, A, Z] = {
		type Acc2 = Acc
		new Repeated[Ctx, A, Z] {
			type Acc = Acc2
			def init()(implicit ctx:Ctx):Acc = initFn(ctx)
			def append(acc:Acc, elem:A)(implicit ctx:Ctx):Acc = appendFn(acc, elem, ctx)
			def result(acc:Acc)(implicit ctx:Ctx):Z = resultFn(acc, ctx)
		}
	}

	/**
	 * Repeated units results in a unit
	 * @group AnyContext
	 */
	implicit def unit:Repeated[Any, Unit, Unit] = {
		Repeated.apply[Any, Unit, Unit, Unit](
			(_) => (),
			(acc, _, _) => acc,
			(acc, _) => acc,
		)
	}

	/**
	 * Creates a String consisting of each of the input Char values in order
	 * @group AnyContext
	 */
	implicit def charToString:Repeated[Any, Char, String] = {
		Repeated.apply[Any, Char, StringBuilder, String](
			(_: Any) => new StringBuilder,
			(acc, elem, _: Any) => acc += elem,
			(acc, _: Any) => acc.toString,
		)
	}

	/**
	 * Creates a String consisting of each of the input CodePoint values in order
	 * @group AnyContext
	 */
	implicit def codepointToString:Repeated[Any, CodePoint, String] = {
		Repeated.apply[Any, CodePoint, java.lang.StringBuilder, String](
			(_: Any) => new java.lang.StringBuilder,
			(acc, elem, _: Any) => acc.appendCodePoint(elem.intValue),
			(acc, _: Any) => acc.toString,
		)
	}

	/**
	 * Creates a String consisting of the concatenation of the component strings
	 * @since 0.1.1
	 * @group AnyContext
	 */
	def idConcatenateString:Repeated[Any, String, String] = {
		Repeated.apply(
			(_: Any) => new StringBuilder,
			(acc:StringBuilder, elem:String, _: Any) => acc ++= elem,
			(acc:StringBuilder, _: Any) => acc.toString,
		)
	}

	/**
	 * @param newAccumulator a Function0 that creates a new Builder
	 * @version 0.1.1
	 * @group IdContext
	 */
	def idFromSplicesUsingBuilder[A, Z](
		newAccumulator: () => Builder[A, Z],
	): Repeated[IdCtx, SplicePiece[Id, A], Z] = {
		final class FromSplicesUsingBuilder extends Repeated[IdCtx, SplicePiece[Id, A], Z] {
			type Acc = Builder[A, Z]
			def init()(implicit ctx: IdCtx): Acc = newAccumulator()
			def append(acc: Acc, piece: SplicePiece[Id, A])(implicit ctx: IdCtx): Acc = {
				piece match {
					case SplicePiece.Zero() =>
						acc
					case SplicePiece.One(elem) =>
						acc.+=(elem)
					case SplicePiece.Many(iter) =>
						acc.++=(iter)
				}
			}
			def result(acc: Acc)(implicit ctx: IdCtx): Z = acc.result()
		}
		new FromSplicesUsingBuilder()
	}

	/**
	 * @version 0.1.1
	 * @group IdContext
	 */
	implicit def idFromSplicesToList[A]: Repeated[IdCtx, SplicePiece[Id, A], List[A]] =
		idFromSplicesUsingBuilder(() => List.newBuilder)


	/**
	 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	def contextConcatenateString[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:Repeated[Ctx, Ctx#Expr[String], Ctx#Expr[String]] = {
		final class ConcatenateString extends Repeated[Ctx, Ctx#Expr[String], Ctx#Expr[String]] {
			type Acc = List[Ctx#Expr[String]]

			def init()(implicit ctx: Ctx):Acc = Nil
			def append(acc:Acc, elem:Ctx#Expr[String])(implicit ctx:Ctx):Acc = elem :: acc
			def result(acc:Acc)(implicit ctx:Ctx):Ctx#Expr[String] = {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._
				import ctx.universe.Quasiquote
				val ttString0: ctx.TypeTag[String] = ctx.typeTag[String]
				locally {
					implicit val ttString: ctx.TypeTag[String] = ttString0

					acc match {
						case List() => ctx.Expr[String](ctx.universe.Literal(ctx.universe.Constant("")))
						case List(elem) => elem
						case _ => {
							val accumulatorTypeTree = ctx.universe.TypeTree(
								ctx.universe.rootMirror.staticClass("scala.collection.mutable.StringBuilder").asType.toTypeConstructor
							)

							val acc2 = acc.map(x => x: ctx.Expr[String])

							val building = acc2.foldRight(q"new $accumulatorTypeTree()")({(part, builder) => q"$builder.append($part)"})
							ctx.Expr[String](q"$building.result()")
						}
					}
				}
			}
		}
		new ConcatenateString()
	}

	/**
	 * Splice a sequence of `SplicePiece`s together using a Builder
	 * @param newAccumulator an Expr creating a new Builder
	 * @param ifZero the Expr to use when combining zero items together.
	 *     If None, `newAccumulator.result()` will be used.
	 *     If Some, should ideally be equivalent to but more efficient than the None expr.
	 * @param ifOneScalar the Expr to use when combining one scalar item together.
	 *     When not defined, `newAccumulator.+=(item).result()` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @param ifOneSplice the Expr to use when combining one splice item together.
	 *     When not defined, `newAccumulator.++=(item).result()` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	def contextFromSplicesUsingBuilder[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A, Z](
			newAccumulator: Ctx => Ctx#Expr[Builder[A, Z]],
			ifZero: Ctx => Option[Ctx#Expr[Z]],
			ifOneScalar: PartialFunction[(Ctx#Expr[A], Ctx), Ctx#Expr[Z]],
			ifOneSplice: PartialFunction[(Ctx#Expr[Iterable[A]], Ctx), Ctx#Expr[Z]],
			)(implicit
			zType: Ctx#TypeTag[Z],
	): Repeated[Ctx, SplicePiece[Ctx#Expr, A], Ctx#Expr[Z]] = {
		// using default arguments confuses the typechecker (found `c.Expr` required `x$1.Expr`), so don't provide default arguments

		final class FromSplicesUsingBuilder extends Repeated[Ctx, SplicePiece[Ctx#Expr, A], Ctx#Expr[Z]] {
			type Acc = List[SplicePiece[Ctx#Expr, A]]

			def init()(implicit ctx:Ctx):Acc = Nil
			def append(acc:Acc, elem:SplicePiece[Ctx#Expr, A])(implicit ctx:Ctx):Acc = {
				elem match {
					case _: SplicePiece.Zero[Ctx#Expr] => acc
					case other => other :: acc
				}
			}

			def result(acc:Acc)(implicit ctx:Ctx):Ctx#Expr[Z] = {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._
				import ctx.universe.Quasiquote

				val newAccumulator2 = (newAccumulator(ctx): ctx.Expr[Builder[A, Z]]).tree

				acc match {
					case Nil =>
						ifZero(ctx).getOrElse(ctx.Expr[Z](q"$newAccumulator2.result()"))
					case List(SplicePiece.One(elem)) =>
						ifOneScalar.applyOrElse((elem, ctx), {(arg: (Ctx#Expr[A], Ctx)) =>
							val (e, _) = arg
							val e2 = e: ctx.Expr[A]
							ctx.Expr[Z](q"$newAccumulator2.addOne($e2).result()"):Ctx#Expr[Z]
						})
					case List(SplicePiece.Many(iter)) =>
						ifOneSplice.applyOrElse((iter, ctx), {(arg: (Ctx#Expr[Iterable[A]], Ctx)) =>
							val (es, _) = arg
							val es2 = es: ctx.Expr[Iterable[A]]
							ctx.Expr[Z](q"$newAccumulator2.addAll($es2).result()"):Ctx#Expr[Z]
						})
					case _ =>
						val acc2 = acc.asInstanceOf[List[SplicePiece[ctx.Expr, A]]]

						val building = acc2.foldRight(newAccumulator2)({(part, builder) =>
							part match {
								case _: SplicePiece.Zero[ctx.Expr] => builder
								case SplicePiece.One(part) => q"$builder.+=($part)"
								case SplicePiece.Many(parts) => q"$builder.++=($parts)"
							}
						})
						ctx.Expr[Z](q"$building.result()")(using zType)
				}
			}
		}
		new FromSplicesUsingBuilder()
	}

	/**
	 * Splice a sequence of `SplicePiece`s together into a `List`
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextFromSplicesToExprList[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A](implicit
			aType: Ctx#TypeTag[A],
			zType: Ctx#TypeTag[List[A]],
	): Repeated[Ctx, SplicePiece[Ctx#Expr, A], Ctx#Expr[List[A]]] = {
		this.contextFromSplicesUsingBuilder[Ctx, A, List[A]](
			ctx => {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._

				import ctx.universe.Quasiquote
				implicit val aType2: ctx.TypeTag[A] = aType
				val retval = ctx.Expr[Builder[A, List[A]]](q"List.newBuilder[$aType2]")
				val retval2 = retval: Ctx#Expr[Builder[A, List[A]]]
				retval2
			},
			ctx => {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._

				import ctx.universe.Quasiquote
				implicit val aType2: ctx.TypeTag[A] = aType
				val retval = Option(ctx.Expr[List[A]](q"List.empty[$aType2]"))
				val retval2 = retval: Option[Ctx#Expr[List[A]]]
				retval2
			},
			{new PartialFunction[(Ctx#Expr[A], Ctx), Ctx#Expr[List[A]]]() {
				override def isDefinedAt(arg: (Ctx#Expr[A], Ctx)): Boolean = true
				override def apply(arg: (Ctx#Expr[A], Ctx)): Ctx#Expr[List[A]] = {
					val (value, ctx) = arg
					val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
					import myBindSingletonContexts._
					import ctx.universe.Quasiquote

					val value2: ctx.Expr[A] = value
					implicit val aType2: ctx.TypeTag[A] = aType
					ctx.Expr[List[A]](q"List[$aType2]($value2)")
				}
			}},
			{new PartialFunction[(Ctx#Expr[Iterable[A]], Ctx), Ctx#Expr[List[A]]]() {
				override def isDefinedAt(arg: (Ctx#Expr[Iterable[A]], Ctx)): Boolean = {
					val (value, _) = arg
					value.staticType <:< zType.tpe
				}
				override def apply(arg: (Ctx#Expr[Iterable[A]], Ctx)): Ctx#Expr[List[A]] = {
					if (this.isDefinedAt(arg)) {
						val (value, ctx) = arg
						val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
						import myBindSingletonContexts._
						val value2: ctx.Expr[Iterable[A]] = value
						implicit val zType2: ctx.TypeTag[List[A]] = zType
						val retval: ctx.Expr[List[A]] = ctx.Expr[List[A]](value2.tree)
						retval
					} else {
						throw new MatchError("PartialFunction not defined at this value")
					}
				}
			}},
		)
	}


	/**
	 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
	 * @since 0.1.1
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedConcatenateString:Repeated[scala.quoted.Quotes, scala.quoted.Expr[String], scala.quoted.Expr[String]] = {
		new RepeatedImpl.ConcatenateString
	}

	/**
	 * Splice a sequence of `SplicePiece`s together using a Builder
	 * @param newAccumulator an Expr creating a new Builder
	 * @param ifZero the Expr to use when combining zero items together.
	 *     If None, `'{$newAccumulator.result()}` will be used.
	 *     If Some, should be equivalent to but more efficient than the None expr.
	 * @param ifOneScalar the Expr to use when combining one scalar item together.
	 *     When not defined, `'{$newAccumulator.addOne($item).result()}` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @param ifOneSplice the Expr to use when combining one splice item together.
	 *     When not defined, `'{$newAccumulator.addAll($item).result()}` will be used.
	 *     The definition should be equivalent to but more efficient than the undefined expr.
	 * @version 0.1.1
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedFromSplicesUsingBuilder[A, Z](
			newAccumulator: (scala.quoted.Quotes) => scala.quoted.Expr[Builder[A, Z]],
			ifZero: (scala.quoted.Quotes) => Option[scala.quoted.Expr[Z]] = _ => None,
			ifOneScalar: PartialFunction[(scala.quoted.Expr[A], scala.quoted.Quotes), scala.quoted.Expr[Z]] = PartialFunction.empty,
			ifOneSplice: PartialFunction[(scala.quoted.Expr[Iterable[A]], scala.quoted.Quotes), scala.quoted.Expr[Z]] = PartialFunction.empty
			)(implicit typA: TypeCreator[A], typZ: TypeCreator[Z],
	): Repeated[scala.quoted.Quotes, SplicePiece[scala.quoted.Expr, A], scala.quoted.Expr[Z]] = {
		new RepeatedImpl.FromSplicesUsingBuilder(newAccumulator, ifZero, ifOneScalar, ifOneSplice)
	}

	/**
	 * Splice a sequence of `SplicePiece`s together into a `List`
	 * @since 0.1.1
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedFromSplicesToExprList[A](implicit typA: TypeCreator[A]): Repeated[scala.quoted.Quotes, SplicePiece[scala.quoted.Expr, A], scala.quoted.Expr[List[A]]] =
		RepeatedImpl.quotedFromSplicesToExprList

	/**
	 * Represents either zero items, one item or a sequence of items.
	 * @version 0.1.1
	 * @group Support
	 */
	sealed trait SplicePiece[Expr[+_], +A]
	/**
	 * The [[SplicePiece]] cases
	 * @version 0.1.1
	 * @group Support
	 */
	object SplicePiece {
		final case class Zero[Expr[+_]]() extends SplicePiece[Expr, Nothing]
		final case class One[Expr[+_], +A](val elem: Expr[A]) extends SplicePiece[Expr, A]
		final case class Many[Expr[+_], +A](val iter: Expr[Iterable[A]]) extends SplicePiece[Expr, A]
	}
}

private[typeclass] trait LowPrioRepeated {
	/**
	 * The fallback Repeated;
	 * creates a List containing each of the input values
	 * @group AnyContext
	 */
	implicit def toList[A]:Repeated[Any, A, List[A]] = {
		Repeated.apply[Any, A, Builder[A, List[A]], List[A]](
			(_:Any) => List.newBuilder[A],
			(acc, elem, _:Any) => acc += elem,
			(acc, _:Any) => acc.result()
		)
	}
}

/**
 * Predefined implicit implementations of ContraRepeated
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
object ContraRepeated extends LowPrioContraRepeated {
	/**
	 * @group IdContext
	 */
	implicit def idUnit:ContraRepeated[IdCtx, Id, Unit, Unit] = BiRepeated.idUnit

	/**
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:ContraRepeated[Ctx, Ctx#Expr, Unit, Unit] =
		BiRepeated.contextUnit

	/**
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextToExprList[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A](implicit tt:Ctx#TypeTag[A]):ContraRepeated[Ctx, Ctx#Expr, Ctx#Expr[A], Ctx#Expr[List[A]]] =
		BiRepeated.contextToExprList[Ctx, A]

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit:ContraRepeated[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit] =
		BiRepeated.quotedUnit

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprList[A](implicit typA: TypeCreator[A]):ContraRepeated[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[List[A]]] =
		BiRepeated.quotedToExprList
}

private[typeclass] trait LowPrioContraRepeated {
	/**
	 * @group IdContext
	 */
	implicit def idToList[A]:ContraRepeated[IdCtx, Id, A, List[A]] = BiRepeated.idToList
}

/**
 * Predefined implicit implementations of BiRepeated
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
object BiRepeated extends LowPrioBiRepeated {
	/**
	 * @group Support
	 */
	private[typeclass] def apply[Ctx, Expr[+_], A, Acc, Z](
		initFn: (Ctx) => Acc,
		appendFn: (Acc, A, Ctx) => Acc,
		resultFn: (Acc, Ctx) => Z,
		headtailFn: PartialExprFunction[Ctx, Expr, Z, (A, Z)],
		isEmptyFn: (Z, Ctx) => Expr[Boolean],
	): BiRepeated[Ctx, Expr, A, Z] = {
		type Acc2 = Acc
		new BiRepeated[Ctx, Expr, A, Z] {
			type Acc = Acc2
			def init()(implicit ctx:Ctx):Acc = initFn(ctx)
			def append(acc:Acc, elem:A)(implicit ctx:Ctx):Acc = appendFn(acc, elem, ctx)
			def result(acc:Acc)(implicit ctx:Ctx):Z = resultFn(acc, ctx)

			def headTail:PartialExprFunction[Ctx, Expr, Z, (A, Z)] = headtailFn
			def isEmpty(it:Z)(implicit ctx:Ctx):Expr[Boolean] = isEmptyFn(it, ctx)
		}
	}

	/**
	 * @group IdContext
	 */
	implicit def idUnit:BiRepeated[IdCtx, Id, Unit, Unit] = {
		BiRepeated.apply[IdCtx, Id, Unit, Unit, Unit](
			(_) => (),
			(acc, _, _) => acc,
			(acc, _) => acc,
			PartialExprFunction[IdCtx, Id, Unit, (Unit, Unit)](
				(_, _) => true,
				(value, _) => (value, value),
			),
			(_, _) => true,
		)
	}

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
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:BiRepeated[Ctx, Ctx#Expr, Unit, Unit] = {
		BiRepeated.apply[Ctx, Ctx#Expr, Unit, Unit, Unit](
			(_) => (),
			(acc, _, _) => acc,
			(acc, _) => acc,
			PartialExprFunction[Ctx, Ctx#Expr, Unit, (Unit, Unit)](
				(_, ctx) => Exprs.forContext[Ctx].constTrue(ctx),
				(value, _) => (value, value)
			),
			(_, ctx) => Exprs.forContext[Ctx].constTrue(ctx),
		)
	}

	/**
	 * @since 0.2.0
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextToExprList[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A](implicit typA:Ctx#TypeTag[A]):BiRepeated[Ctx, Ctx#Expr, Ctx#Expr[A], Ctx#Expr[List[A]]] = {
		BiRepeated.apply[Ctx, Ctx#Expr, Ctx#Expr[A], Builder[Ctx#Tree, List[Ctx#Tree]], Ctx#Expr[List[A]]](
			(_) => List.newBuilder[Ctx#Tree],
			(acc, elem, _) => {acc += elem.tree},
			(acc, ctx) => {
				ctx.Expr[List[A]](
					ctx.universe.Apply(
						selectTermNames[Nothing](ctx)("_root_", "scala", "collection", "immutable", "List", "apply").tree,
						acc.result().asInstanceOf[List[ctx.Tree]]
					)
				): Ctx#Expr[List[A]]
			},
			PartialExprFunction(
				(value, ctx) => {
					val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
					import myBindSingletonContexts._
					@annotation.nowarn("msg=never used") implicit val typA2:ctx.TypeTag[A] = typA
					val value2 = value: ctx.Expr[List[A]]
					select[List[A], Boolean](ctx)(value2, "nonEmpty"): Ctx#Expr[Boolean]
				},
				(value, ctx) => {
					val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
					import myBindSingletonContexts._
					@annotation.nowarn("msg=never used") implicit val typA2:ctx.TypeTag[A] = typA
					val value2 = value: ctx.Expr[List[A]]
					(
						select[List[A], A](ctx)(value2, "head"): Ctx#Expr[A],
						select[List[A], List[A]](ctx)(value2, "tail"): Ctx#Expr[List[A]]
					)
				}
			),
			(value, ctx) => {
				val myBindSingletonContexts = new BindSingletonContexts[Ctx, ctx.type]
				import myBindSingletonContexts._
				@annotation.nowarn("msg=never used") implicit val typA2:ctx.TypeTag[A] = typA
				val value2 = value: ctx.Expr[List[A]]
				select[List[A], Boolean](ctx)(value2, "isEmpty"): Ctx#Expr[Boolean]
			},
		)
	}

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit:BiRepeated[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit] = {
		BiRepeated.apply[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit, Unit](
			(_) => (),
			(acc, _, _) => acc,
			(acc, _) => acc,
			PartialExprFunction[scala.quoted.Quotes, scala.quoted.Expr, Unit, (Unit, Unit)](
				(_, ctx) => {
					implicit val ctx2: scala.quoted.Quotes = ctx
					scala.quoted.Expr(true)
				},
				(value, _) => (value, value),
			),
			(_, ctx) => {
				implicit val ctx2: scala.quoted.Quotes = ctx
				scala.quoted.Expr(true)
			},
		)
	}

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprList[A](implicit typ: TypeCreator[A]):BiRepeated[scala.quoted.Quotes, scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[List[A]]] =
		RepeatedImpl.quotedToExprList
}

private[typeclass] trait LowPrioBiRepeated {
	/**
	 * @group IdContext
	 */
	implicit def idToList[A]:BiRepeated[IdCtx, Id, A, List[A]] = {
		BiRepeated.apply[IdCtx, Id, A, Builder[A, List[A]], List[A]](
			(_: IdCtx) => List.newBuilder[A],
			(acc, elem, _: IdCtx) => {acc += elem},
			(acc, _: IdCtx) => acc.result(),
			PartialExprFunction[IdCtx, Id, List[A], (A, List[A])](
				(it, _: IdCtx) => it.nonEmpty,
				(it, _: IdCtx) => ((it.head, it.tail)),
			),
			(it, _: IdCtx) => it.isEmpty,
		)
	}
}
