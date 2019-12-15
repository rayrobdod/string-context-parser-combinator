package com.rayrobdod.stringContextParserCombinator

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
}