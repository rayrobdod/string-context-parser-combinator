package com.rayrobdod.stringContextParserCombinator

import scala.collection.mutable.Builder

/**
 * Container for type-level logic
 */
object Implicits {
	/** Describes how to combine two adjacent values into one value */
	trait AndThenTypes[-A, -B, +Z] {
		def aggregate(a:A, b:B):Z
	}
	/** Predefined implicit implementations of AndThenTypes */
	object AndThenTypes extends LowPrioAndThenTypes {
		implicit def andThenUnitBoth:AndThenTypes[Unit, Unit, Unit] = new AndThenUnitBoth
		private[this] final class AndThenUnitBoth extends AndThenTypes[Unit, Unit, Unit] {
			def aggregate(a:Unit, b:Unit):Unit = ()
		}
		implicit def andThenUnitLeft[B]:AndThenTypes[Unit, B, B] = new AndThenUnitLeft
		private[this] final class AndThenUnitLeft[B] extends AndThenTypes[Unit, B, B] {
			def aggregate(u:Unit, b:B):B = b
		}
		implicit def andThenUnitRight[A]:AndThenTypes[A, Unit, A] = new AndThenUnitRight
		private[this] final class AndThenUnitRight[A] extends AndThenTypes[A, Unit, A] {
			def aggregate(a:A, u:Unit):A = a
		}
	}
	private[Implicits] trait LowPrioAndThenTypes {
		implicit def andThenGeneric[A,B]:AndThenTypes[A, B, (A, B)] = new AndThenGeneric
		private[this] final class AndThenGeneric[A, B] extends AndThenTypes[A, B, (A,B)] {
			def aggregate(a:A, b:B):(A,B) = (a,b)
		}
	}

	/** Describes the type that represents the repetition of a type */
	trait RepeatTypes[-A, +Z] {
		/** A mutable accumulator appropriate for holding `A` and transforming into `Z` */
		type Acc
		/** Returns an empty accumulator */
		def init():Acc
		/** Inserts `elem` into `acc` */
		def append(acc:Acc, elem:A):Unit
		/** Transforms `acc` into Z */
		def result(acc:Acc):Z
	}
	/** Predefined implicit implementations of RepeatTypes */
	object RepeatTypes extends LowPrioRepeatTypes {
		implicit def repeatUnit:RepeatTypes[Unit, Unit] = new RepeatTypesUnit
		private[this] final class RepeatTypesUnit extends RepeatTypes[Unit, Unit] {
			type Acc = Unit
			def init():Acc = ()
			def append(acc:Acc, elem:Unit):Unit = {}
			def result(acc:Acc):Unit = ()
		}
		implicit def repeatChar:RepeatTypes[Char, String] = new RepeatTypesChar
		private[this] final class RepeatTypesChar extends RepeatTypes[Char, String] {
			type Acc = StringBuilder
			def init():Acc = new StringBuilder
			def append(acc:Acc, elem:Char):Unit = {acc += elem}
			def result(acc:Acc):String = acc.toString
		}
		implicit def repeatCodepoint:RepeatTypes[CodePoint, String] = new RepeatTypesCodepoint
		private[this] final class RepeatTypesCodepoint extends RepeatTypes[CodePoint, String] {
			type Acc = java.lang.StringBuilder
			def init():Acc = new java.lang.StringBuilder
			def append(acc:Acc, elem:CodePoint):Unit = {acc.appendCodePoint(elem.value)}
			def result(acc:Acc):String = acc.toString
		}
	}
	private[Implicits] trait LowPrioRepeatTypes {
		implicit def repeatGenericToList[A]:RepeatTypes[A, List[A]] = new RepeatGenericToList
		private[this] final class RepeatGenericToList[A] extends RepeatTypes[A, List[A]] {
			type Acc = Builder[A, List[A]]
			def init():Acc = List.newBuilder[A]
			def append(acc:Acc, elem:A):Unit = {acc += elem}
			def result(acc:Acc):List[A] = acc.result()
		}
	}

	/** Describes the type that represents the option of a type */
	trait OptionallyTypes[-A, +Z] {
		/** Returns a `Z` value representing a missing `A` */
		def none():Z
		/** Returns a `Z` value representing the given `A` */
		def some(elem:A):Z
	}
	/** Predefined implicit implementations of OptionallyTypes */
	object OptionallyTypes extends LowPrioOptionallyTypes {
		implicit def optionallyUnit:OptionallyTypes[Unit, Unit] = new OptionallyUnit
		private[this] final class OptionallyUnit extends OptionallyTypes[Unit, Unit] {
			def none():Unit = ()
			def some(elem:Unit):Unit = elem
		}
	}
	private[Implicits] trait LowPrioOptionallyTypes {
		implicit def optinallyGeneric[A]:OptionallyTypes[A, Option[A]] = new OptinallyGeneric[A]
		private[this] final class OptinallyGeneric[A] extends OptionallyTypes[A, Option[A]] {
			def none():Option[A] = None
			def some(elem:A):Option[A] = Some(elem)
		}
	}
}
