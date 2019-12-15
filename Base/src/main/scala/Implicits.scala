package com.rayrobdod.stringContextParserCombinator

import scala.collection.mutable.Builder
import scala.collection.immutable.Seq

object Implicits {
	trait AndThenTypes[-A, -B, +Z] {
		def aggregate(a:A, b:B):Z
	}
	object AndThenTypes extends LowPrioAndThenTypes {
		implicit object andThenUnitBoth extends AndThenTypes[Unit, Unit, Unit] {
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
	trait LowPrioAndThenTypes {
		implicit def andThenTypes2[A,B]:AndThenTypes[A, B, (A, B)] = new AndThenTypes2
		private[this] final class AndThenTypes2[A, B] extends AndThenTypes[A, B, (A,B)] {
			def aggregate(a:A, b:B):(A,B) = (a,b)
		}
	}

	trait RepeatTypes[-A, +Z] {
		type Acc
		def init():Acc
		def append(acc:Acc, elem:A):Unit
		def result(acc:Acc):Z
	}
	object RepeatTypes extends LowPrioRepeatTypes {
		implicit object CharRepeatTypes extends RepeatTypes[Char, String] {
			type Acc = StringBuilder
			def init():Acc = new StringBuilder
			def append(acc:Acc, elem:Char):Unit = {acc += elem}
			def result(acc:Acc):String = acc.toString
		}
		implicit object CodePointRepeatTypes extends RepeatTypes[CodePoint, String] {
			type Acc = java.lang.StringBuilder
			def init():Acc = new java.lang.StringBuilder
			def append(acc:Acc, elem:CodePoint):Unit = {acc.appendCodePoint(elem.value)}
			def result(acc:Acc):String = acc.toString
		}
	}
	trait LowPrioRepeatTypes {
		implicit def seqRepeatTypes[A]:RepeatTypes[A, List[A]] = new SeqRepeatTypes
		private[this] final class SeqRepeatTypes[A] extends RepeatTypes[A, List[A]] {
			type Acc = Builder[A, List[A]]
			def init():Acc = List.newBuilder[A]
			def append(acc:Acc, elem:A):Unit = {acc += elem}
			def result(acc:Acc):List[A] = acc.result()
		}
	}
}