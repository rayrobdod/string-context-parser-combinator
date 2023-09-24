package name.rayrobdod.stringContextParserCombinatorExample.datetime

import name.rayrobdod.stringContextParserCombinator.typeclass.Repeated

/** Represents a base-ten digit. */
final class Digit private (private val value:Int)
/** Represents a sequence of base-ten digits. */
final class Digits(val value:Int)

object Digit {
	def apply(x:Char):Digit = {
		if ('0' <= x && x <= '9') {
			new Digit(x - '0')
		} else {
			throw new IllegalArgumentException("Expected ascii digit")
		}
	}

	implicit def given_Repeated:Repeated[Digit, Digits] = new Repeated[Digit, Digits]{
		type Acc = Int
		def init():Acc = 0
		def append(acc:Acc, elem:Digit):Acc = (acc * 10) + elem.value
		def result(acc:Acc):Digits = new Digits(acc)
	}
}
