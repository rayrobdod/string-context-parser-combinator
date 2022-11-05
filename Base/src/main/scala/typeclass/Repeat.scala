package com.rayrobdod.stringContextParserCombinator
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

/** Predefined implicit implementations of Repeated */
object Repeated extends LowPrioRepeated {
	implicit def repeatedUnit:Repeated[Unit, Unit] = {
		final class RepeatedUnit extends Repeated[Unit, Unit] {
			type Acc = Unit
			def init():Acc = ()
			def append(acc:Acc, elem:Unit):Unit = {}
			def result(acc:Acc):Unit = ()
		}
		new RepeatedUnit()
	}
	implicit def repeatedChar:Repeated[Char, String] = {
		final class RepeatedChar extends Repeated[Char, String] {
			type Acc = StringBuilder
			def init():Acc = new StringBuilder
			def append(acc:Acc, elem:Char):Unit = {acc += elem}
			def result(acc:Acc):String = acc.toString
		}
		new RepeatedChar()
	}
	implicit def repeatedCodepoint:Repeated[CodePoint, String] = {
		final class RepeatedCodepoint extends Repeated[CodePoint, String] {
			type Acc = java.lang.StringBuilder
			def init():Acc = new java.lang.StringBuilder
			def append(acc:Acc, elem:CodePoint):Unit = {acc.appendCodePoint(elem.value)}
			def result(acc:Acc):String = acc.toString
		}
		new RepeatedCodepoint()
	}
}

private[typeclass] trait LowPrioRepeated {
	implicit def repeatedGenericToList[A]:Repeated[A, List[A]] = {
		final class RepeatedGenericToList extends Repeated[A, List[A]] {
			type Acc = Builder[A, List[A]]
			def init():Acc = List.newBuilder[A]
			def append(acc:Acc, elem:A):Unit = {acc += elem}
			def result(acc:Acc):List[A] = acc.result()
		}
		new RepeatedGenericToList()
	}
}
