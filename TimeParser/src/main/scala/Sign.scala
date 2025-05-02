package name.rayrobdod.stringContextParserCombinatorExample.datetime

import name.rayrobdod.stringContextParserCombinator.typeclass.Sequenced

final class Sign(isPositive:Boolean) {
	def *(rhs:Int) = if (this.isPositive) {rhs} else {-rhs}
}

object Sign {
	val Positive = new Sign(true)
	val Negative = new Sign(false)

	implicit def given_Sequenced_Sign_Digit:Sequenced[Any, Sign, Digits, Int] = new Sequenced[Any, Sign, Digits, Int]{
		def aggregate(sign:Sign, digits:Digits)(implicit ctx:Any) = sign * digits.value
	}
}
