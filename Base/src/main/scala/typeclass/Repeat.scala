package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder

/**
 * Describes how to combine a homogeneous sequence of zero-or-more values.
 *
 * When a Repeated is used:
 *  * first, `init` is called to create a new mutable builder
 *  * then, `append` is called once for each component item in order, using the `init`-created builder and the component item as parameters
 *  * lastly, `result` is called with the builder, and the result of this call is overall result.
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
 * 	final class Box(var value:Int)
 * 	type Acc = Box
 * 	def init():Acc = new Box(0)
 * 	def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
 * 	def result(acc:Acc):Digits = new Digits(acc.value)
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
	/** A mutable accumulator appropriate for holding `A` and transforming into `Z` */
	type Acc
	/** Returns a new empty accumulator */
	def init():Acc
	/** Inserts `elem` into `acc` */
	def append(acc:Acc, elem:A):Unit
	/** Transforms `acc` into Z */
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
	/**
	 * Repeated units results in a unit
	 */
	implicit def unit:Repeated[Unit, Unit] = {
		final class RepeatedUnit extends Repeated[Unit, Unit] {
			type Acc = Unit
			def init():Acc = ()
			def append(acc:Acc, elem:Unit):Unit = {}
			def result(acc:Acc):Unit = ()
		}
		new RepeatedUnit()
	}

	/**
	 * Creates a String consisting of each of the input Char values in order
	 */
	implicit def charToString:Repeated[Char, String] = {
		final class RepeatedChar extends Repeated[Char, String] {
			type Acc = StringBuilder
			def init():Acc = new StringBuilder
			def append(acc:Acc, elem:Char):Unit = {acc += elem; ()}
			def result(acc:Acc):String = acc.toString
		}
		new RepeatedChar()
	}

	/**
	 * Creates a String consisting of each of the input CodePoint values in order
	 */
	implicit def codepointToString:Repeated[CodePoint, String] = {
		final class RepeatedCodepoint extends Repeated[CodePoint, String] {
			type Acc = java.lang.StringBuilder
			def init():Acc = new java.lang.StringBuilder
			def append(acc:Acc, elem:CodePoint):Unit = {acc.appendCodePoint(elem.intValue); ()}
			def result(acc:Acc):String = acc.toString
		}
		new RepeatedCodepoint()
	}
}

private[typeclass] trait LowPrioRepeated {
	/**
	 * The fallback Repeated;
	 * creates a List containing each of the input values
	 */
	implicit def toList[A]:Repeated[A, List[A]] = {
		final class RepeatedGenericToList extends Repeated[A, List[A]] {
			type Acc = Builder[A, List[A]]
			def init():Acc = List.newBuilder[A]
			def append(acc:Acc, elem:A):Unit = {acc += elem; ()}
			def result(acc:Acc):List[A] = acc.result()
		}
		new RepeatedGenericToList()
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
	implicit def idUnit:BiRepeated[Id, Unit, Unit] = {
		new BiRepeated[Id, Unit, Unit] {
			type Acc = Unit
			def init():Acc = ()
			def append(acc:Acc, elem:Unit):Unit = {}
			def result(acc:Acc):Unit = ()

			def headTail:PartialExprFunction[Id, Unit, (Unit, Unit)] = {
				PartialExprFunction[Id, Unit, (Unit, Unit)](
					_ => true,
					value => (value, value)
				)
			}
			def isEmpty(it:Unit):Id[Boolean] = true
		}
	}
}

private[typeclass] trait LowPrioBiRepeated extends VersionSpecificLowPrioBiRepeated {
	implicit def idToList[A]:BiRepeated[Id, A, List[A]] = {
		new BiRepeated[Id, A, List[A]] {
			type Acc = Builder[A, List[A]]
			def init():Acc = List.newBuilder[A]
			def append(acc:Acc, elem:A):Unit = {acc += elem; ()}
			def result(acc:Acc):List[A] = acc.result()

			def headTail:PartialExprFunction[Id, List[A], (A, List[A])] = {
				PartialExprFunction[Id, List[A], (A, List[A])](
					it => it.nonEmpty,
					it => ((it.head, it.tail))
				)
			}
			def isEmpty(it:List[A]):Boolean = it.isEmpty
		}
	}
}
