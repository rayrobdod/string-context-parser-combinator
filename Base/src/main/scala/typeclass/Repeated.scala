package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef
import scala.collection.mutable.Builder

/**
 * Describes how to combine a homogeneous sequence of zero-or-more values.
 *
 * When a Repeated is used:
 *  * first, `init` to create an initial value for the accumulator
 *  * then, `append` is called once for each component item in order, using the accumulator and the component item as parameters and returning the next accumulator value
 *  * lastly, `result` is called with the final accumulator value, and the result of this call is overall result.
 *
 * `init` will be called anew on each use, so it is possible to use a mutable accumulator
 * by creating a new builder in the `init` method
 * and returning the `acc` parameter in the append method.
 *
 * Below is an example of implementing and using a custom `Repeated`:
 * ```scala
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.charIn
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Repeated
 *
 * // define the marker types
 * case class Digit(value:Int)
 * case class Digits(value:Int)
 *
 * // define the given instance
 * given Repeated[Digit, Digits] with {
 *  type Acc = Int
 *  def init():Acc = 0
 *  def append(acc:Acc, elem:Digit):Acc = (acc * 10) + elem.value
 *  def result(acc:Acc):Digits = new Digits(acc)
 * }
 *
 * // create the parsers
 * val digit:idInterpolators.Interpolator[Digit] = charIn('0' to '9').map(x => Digit(x - '0'))
 * val digits:idInterpolators.Interpolator[Digits] = digit.repeat(1)// using Repeated[Digit, Digits]
 *
 * // use the parser
 * digits.interpolate(StringContext("1234"), Nil) // Digits(1234): Digits
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.repeat Interpolator.repeat]]
 * @tparam A the repeated input elements
 * @tparam Z the result container
 */
trait Repeated[-A, +Z] {
	/** The accumulator */
	type Acc
	/** Returns a new empty accumulator */
	def init():Acc
	/** Inserts `elem` into `acc` */
	def append(acc:Acc, elem:A):Acc
	/** Transforms `acc` into a Z */
	def result(acc:Acc):Z
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
trait ContraRepeated[+Expr[_], +A, Z] {
	def headTail:PartialExprFunction[Expr, Z, (A, Z)]
	def isEmpty(it:Z):Expr[Boolean]
}

/**
 * Describes how to combine and break apart a repeated value
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.repeat Parser.repeat]]
 * @tparam A the repeated input elements
 * @tparam Z the result container
 * @tparam Expr the macro-level expression type
 */
trait BiRepeated[Expr[_], A, Z]
	extends Repeated[A, Z]
	with ContraRepeated[Expr, A, Z]

/** Predefined implicit implementations of Repeated */
object Repeated extends LowPrioRepeated {
	private[typeclass] def apply[A, Acc, Z](
		initFn: () => Acc,
		appendFn: (Acc, A) => Acc,
		resultFn: Acc => Z,
	): Repeated[A, Z] = {
		type Acc2 = Acc
		new Repeated[A, Z] {
			type Acc = Acc2
			def init():Acc = initFn()
			def append(acc:Acc, elem:A):Acc = appendFn(acc, elem)
			def result(acc:Acc):Z = resultFn(acc)
		}
	}

	/**
	 * Repeated units results in a unit
	 */
	implicit def unit:Repeated[Unit, Unit] = {
		Repeated.apply[Unit, Unit, Unit](
			() => (),
			(acc, _) => acc,
			(acc) => acc,
		)
	}

	/**
	 * Creates a String consisting of each of the input Char values in order
	 */
	implicit def charToString:Repeated[Char, String] = {
		Repeated.apply[Char, StringBuilder, String](
			() => new StringBuilder,
			(acc, elem) => acc += elem,
			(acc) => acc.toString,
		)
	}

	/**
	 * Creates a String consisting of each of the input CodePoint values in order
	 */
	implicit def codepointToString:Repeated[CodePoint, String] = {
		Repeated.apply[CodePoint, java.lang.StringBuilder, String](
			() => new java.lang.StringBuilder,
			(acc, elem) => acc.appendCodePoint(elem.intValue),
			(acc) => acc.toString,
		)
	}

	/**
	 * Creates a String consisting of the concatenation of the component strings
	 * @since 0.1.1
	 */
	def idConcatenateString:Repeated[String, String] = {
		Repeated.apply(
			() => new StringBuilder,
			(acc:StringBuilder, elem:String) => acc ++= elem,
			(acc:StringBuilder) => acc.toString,
		)
	}

	/**
	 * @param newAccumulator a Function0 that creates a new Builder
	 * @version 0.1.1
	 */
	def idFromSplicesUsingBuilder[A, Z](
		newAccumulator: () => Builder[A, Z],
	): Repeated[SplicePiece[Id, A], Z] = {
		final class FromSplicesUsingBuilder extends Repeated[SplicePiece[Id, A], Z] {
			type Acc = Builder[A, Z]
			def init(): Acc = newAccumulator()
			def append(acc: Acc, piece: SplicePiece[Id, A]): Acc = {
				piece match {
					case SplicePiece.Zero() =>
						acc
					case SplicePiece.One(elem) =>
						acc.+=(elem)
					case SplicePiece.Many(iter) =>
						acc.++=(iter)
				}
			}
			def result(acc: Acc): Z = acc.result()
		}
		new FromSplicesUsingBuilder()
	}

	/**
	 * @version 0.1.1
	 */
	implicit def idFromSplicesToList[A]: Repeated[SplicePiece[Id, A], List[A]] =
		idFromSplicesUsingBuilder(() => List.newBuilder)

	/**
	 * @since 0.1.1
	 */
	@ifdef("scalaEpochVersion:2")
	trait Repeateds[Expr[+_], Type[_]] {
		/**
		 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
		 * @since 0.1.1
		 */
		def concatenateString: Repeated[Expr[String], Expr[String]]

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
		 * @version 0.1.1
		 */
		def fromSplicesUsingBuilder[A, Z](
				newAccumulator: Expr[Builder[A, Z]],
				ifZero: Option[() => Expr[Z]],
				ifOneScalar: PartialFunction[Expr[A], Expr[Z]],
				ifOneSplice: PartialFunction[Expr[Iterable[A]], Expr[Z]],
				)(implicit
				accumulatorType: Type[Builder[A, Z]],
				zType: Type[Z],
		): Repeated[SplicePiece[Expr, A], Expr[Z]]

		/**
		 * Splice a sequence of `SplicePiece`s together into a `List`
		 * @version 0.1.1
		 */
		implicit def fromSplicesToExprList[A](implicit
				accumulatorType: Type[Builder[A, List[A]]],
				zType: Type[List[A]],
		): Repeated[SplicePiece[Expr, A], Expr[List[A]]]
	}

	/**
	 * @since 0.1.1
	 */
	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):Repeateds[c.Expr, c.TypeTag] = {
		new Repeateds[c.Expr, c.TypeTag] {
			def concatenateString:Repeated[c.Expr[String], c.Expr[String]] = {
				import c.universe.Tree
				import c.universe.Quasiquote
				val ttString0 = c.universe.typeTag[String]
				final class ConcatenateString extends Repeated[c.Expr[String], c.Expr[String]] {
					val accumulatorName = c.freshName(c.universe.TermName("accumulator$"))
					val accumulatorTypeTree = c.universe.TypeTree(
						c.universe.rootMirror.staticClass("scala.collection.mutable.StringBuilder").asType.toTypeConstructor
					)
					val accumulatorIdent = c.universe.Ident(accumulatorName)
					val accumulatorValDef = c.universe.ValDef(
						c.universe.NoMods,
						accumulatorName,
						accumulatorTypeTree,
						q"new $accumulatorTypeTree()",
					)

					implicit val ttString: c.TypeTag[String] = ttString0

					sealed trait Acc
					final object AccZero extends Acc
					final class AccOne(val elem: c.Expr[String]) extends Acc
					final class AccMany extends Acc {
						val builder: Builder[Tree, c.Expr[String]] = List.newBuilder.mapResult(stat =>
							c.Expr[String](
								c.universe.Block(
									stat,
									q"$accumulatorIdent.toString"
								)
							)
						)
					}

					def init():Acc = AccZero
					def append(acc:Acc, elem:c.Expr[String]):Acc = acc match {
						case AccZero => new AccOne(elem)
						case accOne: AccOne => {
							val retval = new AccMany()
							retval.builder += accumulatorValDef
							retval.builder += q"$accumulatorIdent.append(${accOne.elem})"
							retval.builder += q"$accumulatorIdent.append($elem)"
							retval
						}
						case accMany: AccMany => {
							accMany.builder += q"$accumulatorIdent.append($elem)"
							accMany
						}
					}
					def result(acc:Acc):c.Expr[String] = {
						acc match {
							case AccZero => c.Expr[String](c.universe.Literal(c.universe.Constant("")))
							case accOne: AccOne => accOne.elem
							case accMany: AccMany => accMany.builder.result()
						}
					}
				}
				new ConcatenateString()
			}

			def fromSplicesUsingBuilder[A, Z](
					newAccumulator: c.Expr[Builder[A, Z]],
					ifZero: Option[() => c.Expr[Z]],
					ifOneScalar: PartialFunction[c.Expr[A], c.Expr[Z]],
					ifOneSplice: PartialFunction[c.Expr[Iterable[A]], c.Expr[Z]],
					)(implicit
					accumulatorType: c.universe.TypeTag[Builder[A, Z]],
					zType: c.universe.TypeTag[Z],
			): Repeated[SplicePiece[c.Expr, A], c.Expr[Z]] = {
				// using default arguments confuses the typechecker (found `c.Expr` required `x$1.Expr`), so don't provide default arguments

				import c.universe.Tree
				import c.universe.Quasiquote

				final class FromSplicesUsingBuilder extends Repeated[SplicePiece[c.Expr, A], c.Expr[Z]] {
					val accumulatorName = c.freshName(c.universe.TermName("accumulator$"))
					val accumulatorTypeTree = c.universe.TypeTree(accumulatorType.tpe)
					val accumulatorIdent = c.universe.Ident(accumulatorName)
					val accumulatorExpr = c.Expr(accumulatorIdent)(accumulatorType)

					val accumulatorValDef = c.universe.ValDef(
						c.universe.NoMods,
						accumulatorName,
						accumulatorTypeTree,
						newAccumulator.tree
					)

					sealed trait Acc
					final object AccZero extends Acc
					final class AccOneScalar(val elem: c.Expr[A]) extends Acc
					final class AccOneSplice(val iter: c.Expr[Iterable[A]]) extends Acc
					final class AccMany extends Acc {
						val builder: Builder[Tree, c.Expr[Z]] = List.newBuilder.mapResult(stat =>
							c.Expr[Z](
								c.universe.Block(
									stat,
									q"$accumulatorIdent.result()"
								)
							)
						)
					}

					def init():Acc = AccZero
					def append(acc:Acc, elem:SplicePiece[c.Expr, A]):Acc = acc match {
						case AccZero =>
							elem match {
								case _: SplicePiece.Zero[c.Expr] => AccZero
								case elemOne: SplicePiece.One[c.Expr, A] => new AccOneScalar(elemOne.elem)
								case elemMany: SplicePiece.Many[c.Expr, A] => new AccOneSplice(elemMany.iter)
							}
						case accOne: AccOneScalar => {
							elem match {
								case _: SplicePiece.Zero[c.Expr] => accOne
								case elemOne: SplicePiece.One[c.Expr, A] =>
									val retval = new AccMany()
									retval.builder += accumulatorValDef
									retval.builder += q"$accumulatorIdent.+=(${accOne.elem})"
									retval.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
									retval
								case elemMany: SplicePiece.Many[c.Expr, A] =>
									val retval = new AccMany()
									retval.builder += accumulatorValDef
									retval.builder += q"$accumulatorIdent.+=(${accOne.elem})"
									retval.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
									retval
							}
						}
						case accOne: AccOneSplice => {
							elem match {
								case _: SplicePiece.Zero[c.Expr] => accOne
								case elemOne: SplicePiece.One[c.Expr, A] =>
									val retval = new AccMany()
									retval.builder += accumulatorValDef
									retval.builder += q"$accumulatorIdent.++=(${accOne.iter})"
									retval.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
									retval
								case elemMany: SplicePiece.Many[c.Expr, A] =>
									val retval = new AccMany()
									retval.builder += accumulatorValDef
									retval.builder += q"$accumulatorIdent.++=(${accOne.iter})"
									retval.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
									retval
							}
						}
						case accMany: AccMany => {
							elem match {
								case _: SplicePiece.Zero[c.Expr] =>
									// do nothing
								case elemOne: SplicePiece.One[c.Expr, A] =>
									accMany.builder += q"$accumulatorIdent.+=(${elemOne.elem})"
								case elemMany: SplicePiece.Many[c.Expr, A] =>
									accMany.builder += q"$accumulatorIdent.++=(${elemMany.iter})"
							}
							accMany
						}
					}

					def result(acc:Acc):c.Expr[Z] = {
						acc match {
							case AccZero => ifZero.map(_.apply()).getOrElse(c.Expr[Z](q"$newAccumulator.result()"))
							case accOne: AccOneScalar => ifOneScalar.applyOrElse(accOne.elem, (e: c.Expr[A]) => c.Expr[Z](q"$newAccumulator.+=(${e}).result()"))
							case accOne: AccOneSplice => ifOneSplice.applyOrElse(accOne.iter, (es: c.Expr[Iterable[A]]) => c.Expr[Z](q"$newAccumulator.++=(${es}).result()"))
							case accMany: AccMany => accMany.builder.result()
						}
					}

				}
				new FromSplicesUsingBuilder()
			}

			def fromSplicesToExprList[A](implicit
					accumulatorType: c.universe.TypeTag[Builder[A, List[A]]],
					zType: c.universe.TypeTag[List[A]],
			): Repeated[SplicePiece[c.Expr, A], c.Expr[List[A]]] = {
				this.fromSplicesUsingBuilder[A, List[A]](
					c.universe.reify(List.newBuilder),
					Option(() => c.universe.reify(List.empty)),
					{case (a: c.Expr[A]) => c.universe.reify(List(a.splice))},
					{case (a: c.Expr[_]) if a.staticType <:< zType.tpe => c.Expr[List[A]](a.tree)},
				)
			}
		}
	}

	/**
	 * Creates an Expr[String] consisting of the concatenation of the component Expr[String]s
	 * @since 0.1.1
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedConcatenateString(implicit quotes: scala.quoted.Quotes):Repeated[scala.quoted.Expr[String], scala.quoted.Expr[String]] = {
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
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedFromSplicesUsingBuilder[A, Z](
			newAccumulator: scala.quoted.Expr[Builder[A, Z]],
			ifZero: Option[() => scala.quoted.Expr[Z]] = None,
			ifOneScalar: PartialFunction[scala.quoted.Expr[A], scala.quoted.Expr[Z]] = PartialFunction.empty,
			ifOneSplice: PartialFunction[scala.quoted.Expr[Iterable[A]], scala.quoted.Expr[Z]] = PartialFunction.empty
			)(implicit quotes: scala.quoted.Quotes, typA: scala.quoted.Type[A], typZ: scala.quoted.Type[Z], typBuilder: scala.quoted.Type[Builder[A, Z]],
	): Repeated[SplicePiece[scala.quoted.Expr, A], scala.quoted.Expr[Z]] = {
		new RepeatedImpl.FromSplicesUsingBuilder(newAccumulator, ifZero, ifOneScalar, ifOneSplice)
	}

	/**
	 * Splice a sequence of `SplicePiece`s together into a `List`
	 * @since 0.1.1
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedFromSplicesToExprList[A](implicit quotes: scala.quoted.Quotes, typA: scala.quoted.Type[A]): Repeated[SplicePiece[scala.quoted.Expr, A], scala.quoted.Expr[List[A]]] =
		RepeatedImpl.quotedFromSplicesToExprList

	/**
	 * Represents either zero items, one item or a sequence of items.
	 * @version 0.1.1
	 */
	sealed trait SplicePiece[Expr[+_], +A]
	/**
	 * The [[SplicePiece]] cases
	 * @version 0.1.1
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
	 */
	implicit def toList[A]:Repeated[A, List[A]] = {
		Repeated.apply[A, Builder[A, List[A]], List[A]](
			() => List.newBuilder[A],
			(acc, elem) => acc += elem,
			(acc) => acc.result()
		)
	}
}

/** Predefined implicit implementations of ContraRepeated */
object ContraRepeated extends LowPrioContraRepeated {
	implicit def idUnit:ContraRepeated[Id, Unit, Unit] = BiRepeated.idUnit

	@ifdef("scalaEpochVersion:2")
	trait ContraRepeateds[Expr[_], Type[_]] {
		implicit def unit:ContraRepeated[Expr, Unit, Unit]
		implicit def toExprList[A](implicit tt:Type[A]):ContraRepeated[Expr, Expr[A], Expr[List[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):ContraRepeateds[c.Expr, c.TypeTag] = {
		new ContraRepeateds[c.Expr, c.TypeTag] {
			implicit override def unit:ContraRepeated[c.Expr, Unit, Unit] =
				BiRepeated.forContext(c).unit

			implicit override def toExprList[A](implicit tt:c.TypeTag[A]):ContraRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] =
				BiRepeated.forContext(c).toExprList[A]
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit(implicit quotes: scala.quoted.Quotes):ContraRepeated[scala.quoted.Expr, Unit, Unit] =
		BiRepeated.quotedUnit

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprList[A](implicit quotes: scala.quoted.Quotes, typA: scala.quoted.Type[A]):ContraRepeated[scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[List[A]]] =
		BiRepeated.quotedToExprList
}

private[typeclass] trait LowPrioContraRepeated {
	implicit def idToList[A]:ContraRepeated[Id, A, List[A]] = BiRepeated.idToList
}

/** Predefined implicit implementations of BiRepeated */
object BiRepeated extends LowPrioBiRepeated {
	private[typeclass] def apply[Expr[_], A, Acc, Z](
		initFn: () => Acc,
		appendFn: (Acc, A) => Acc,
		resultFn: Acc => Z,
		headtailFn: PartialExprFunction[Expr, Z, (A, Z)],
		isEmptyFn: Z => Expr[Boolean],
	): BiRepeated[Expr, A, Z] = {
		type Acc2 = Acc
		new BiRepeated[Expr, A, Z] {
			type Acc = Acc2
			def init():Acc = initFn()
			def append(acc:Acc, elem:A):Acc = appendFn(acc, elem)
			def result(acc:Acc):Z = resultFn(acc)

			def headTail:PartialExprFunction[Expr, Z, (A, Z)] = headtailFn
			def isEmpty(it:Z):Expr[Boolean] = isEmptyFn(it)
		}
	}

	implicit def idUnit:BiRepeated[Id, Unit, Unit] = {
		BiRepeated.apply[Id, Unit, Unit, Unit](
			() => (),
			(acc, _) => acc,
			(acc) => acc,
			PartialExprFunction[Id, Unit, (Unit, Unit)](
				_ => true,
				value => (value, value),
			),
			_ => true,
		)
	}

	@ifdef("scalaEpochVersion:2")
	trait BiRepeateds[Expr[_], Type[_]] {
		implicit def unit:BiRepeated[Expr, Unit, Unit]
		implicit def toExprList[A](implicit typA:Type[A]):BiRepeated[Expr, Expr[A], Expr[List[A]]]
	}

	@ifdef("scalaEpochVersion:2")
	def forContext(c:scala.reflect.macros.blackbox.Context):BiRepeateds[c.Expr, c.TypeTag] = {
		new BiRepeateds[c.Expr, c.TypeTag] {
			private[this] val exprTrue = c.Expr[Boolean](c.universe.Literal(c.universe.Constant(true)))
			private[this] def select[A, Z](qualifier:c.Expr[A], name:String)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				c.Expr[Z](c.universe.Select(qualifier.tree, c.universe.TermName(name)))
			}
			private[this] def selectTermNames[Z](root:String, names:String*)(implicit typZ:c.TypeTag[Z]):c.Expr[Z] = {
				val rootTree = c.universe.Ident(c.universe.TermName(root))
				val namesTree = names.foldLeft[c.universe.Tree](rootTree)({(folding, name) => c.universe.Select(folding, c.universe.TermName(name))})
				c.Expr[Z](namesTree)
			}

			implicit override def unit:BiRepeated[c.Expr, Unit, Unit] = {
				BiRepeated.apply[c.Expr, Unit, Unit, Unit](
					() => (),
					(acc, _) => acc,
					(acc) => acc,
					PartialExprFunction[c.Expr, Unit, (Unit, Unit)](
						_ => exprTrue,
						value => (value, value)
					),
					_ => exprTrue,
				)
			}

			implicit override def toExprList[A](implicit typA:c.TypeTag[A]):BiRepeated[c.Expr, c.Expr[A], c.Expr[List[A]]] = {
				BiRepeated.apply[c.Expr, c.Expr[A], Builder[c.Tree, List[c.Tree]], c.Expr[List[A]]](
					() => List.newBuilder[c.Tree],
					(acc, elem) => {acc += elem.tree},
					(acc) => {
						c.Expr[List[A]](
							c.universe.Apply(
								selectTermNames[Nothing]("_root_", "scala", "collection", "immutable", "List", "apply").tree,
								acc.result()
							)
						)
					},
					PartialExprFunction(
						it => select[List[A], Boolean](it, "nonEmpty"),
						it => (
							select[List[A], A](it, "head"),
							select[List[A], List[A]](it, "tail")
						)
					),
					it => select[List[A], Boolean](it, "isEmpty"),
				)
			}
		}
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnit(implicit quotes: scala.quoted.Quotes):BiRepeated[scala.quoted.Expr, Unit, Unit] = {
		BiRepeated.apply[scala.quoted.Expr, Unit, Unit, Unit](
			() => (),
			(acc, _) => acc,
			(acc) => acc,
			PartialExprFunction[scala.quoted.Expr, Unit, (Unit, Unit)](
				_ => scala.quoted.Expr(true),
				value => (value, value),
			),
			_ => scala.quoted.Expr(true),
		)
	}

	@ifdef("scalaBinaryVersion:3")
	implicit def quotedToExprList[A](implicit quotes: scala.quoted.Quotes, typ: scala.quoted.Type[A]):BiRepeated[scala.quoted.Expr, scala.quoted.Expr[A], scala.quoted.Expr[List[A]]] =
		RepeatedImpl.quotedToExprList
}

private[typeclass] trait LowPrioBiRepeated {
	implicit def idToList[A]:BiRepeated[Id, A, List[A]] = {
		BiRepeated.apply[Id, A, Builder[A, List[A]], List[A]](
			() => List.newBuilder[A],
			(acc, elem) => {acc += elem},
			(acc) => acc.result(),
			PartialExprFunction[Id, List[A], (A, List[A])](
				it => it.nonEmpty,
				it => ((it.head, it.tail)),
			),
			it => it.isEmpty,
		)
	}
}
