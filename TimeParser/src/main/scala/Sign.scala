package com.rayrobdod.stringContextParserCombinatorExample.datetime

import com.rayrobdod.stringContextParserCombinator.typelevel.Sequenced

final class Sign(isPositive:Boolean) {
	def *(rhs:Int) = if (this.isPositive) {rhs} else {-rhs}
}

object Sign {
	val Positive = new Sign(true)
	val Negative = new Sign(false)

	implicit def given_Sequenced_Sign_Digit:Sequenced[Sign, Digits, Int] = new Sequenced[Sign, Digits, Int]{
		def aggregate(sign:Sign, digits:Digits) = sign * digits.value
	}
}
