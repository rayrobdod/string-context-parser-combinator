package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
private[stringContextParserCombinator]
final case class ExpectingDescription(value:String) {
	override def toString = value

	def where(condition:ExpectingDescription) = new ExpectingDescription(s"$value where ${condition.value}")
}

private[stringContextParserCombinator]
final case class Expecting[Pos](val description:ExpectingDescription, val position:Pos) {
	def where(condition:ExpectingDescription) = new Expecting(description.where(condition), position)
}
