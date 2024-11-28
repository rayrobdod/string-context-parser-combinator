package name.rayrobdod.stringContextParserCombinator
package typeclass

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
object Repeated extends VersionSpecificRepeated with LowPrioRepeated {
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
object ContraRepeated extends VersionSpecificContraRepeated with LowPrioContraRepeated {
	implicit def idUnit:ContraRepeated[Id, Unit, Unit] = BiRepeated.idUnit
}

private[typeclass] trait LowPrioContraRepeated extends VersionSpecificLowPrioContraRepeated {
	implicit def idToList[A]:ContraRepeated[Id, A, List[A]] = BiRepeated.idToList
}

/** Predefined implicit implementations of BiRepeated */
object BiRepeated extends VersionSpecificBiRepeated with LowPrioBiRepeated {
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
}

private[typeclass] trait LowPrioBiRepeated extends VersionSpecificLowPrioBiRepeated {
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
