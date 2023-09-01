package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder

/**
 * Describes how to combine a repeated value
 *
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
 * Describes how to separate a value into its parts
 *
 * The parser determines how many parts a value has
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 */
trait ContraRepeated[+Expr[_], +A, Z] {
	def headTail:PartialExprFunction[Expr, Z, (A, Z)]
	def isEmpty(it:Z):Expr[Boolean]
}

trait BiRepeated[Expr[_], A, Z]
	extends Repeated[A, Z]
	with ContraRepeated[Expr, A, Z]

/** Predefined implicit implementations of Repeated */
object Repeated extends LowPrioRepeated {
	implicit def unit:Repeated[Unit, Unit] = {
		final class RepeatedUnit extends Repeated[Unit, Unit] {
			type Acc = Unit
			def init():Acc = ()
			def append(acc:Acc, elem:Unit):Unit = {}
			def result(acc:Acc):Unit = ()
		}
		new RepeatedUnit()
	}
	implicit def charToString:Repeated[Char, String] = {
		final class RepeatedChar extends Repeated[Char, String] {
			type Acc = StringBuilder
			def init():Acc = new StringBuilder
			def append(acc:Acc, elem:Char):Unit = {acc += elem; ()}
			def result(acc:Acc):String = acc.toString
		}
		new RepeatedChar()
	}
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
