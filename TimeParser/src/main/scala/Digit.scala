package com.rayrobdod.stringContextParserCombinatorExample.datetime

import com.rayrobdod.stringContextParserCombinator.typelevel.Repeated

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
		final class Acc(var value:Int)
		def init():Acc = new Acc(0)
		def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
		def result(acc:Acc):Digits = new Digits(acc.value)
	}
}
