package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
// opaque type ExpectingDescription = String?
private[stringContextParserCombinator]
final case class ExpectingDescription(value:String) {
	override def toString = value

	def where(condition:ExpectingDescription) = new ExpectingDescription(s"$value where ${condition.value}")
}

private[stringContextParserCombinator]
final case class Expecting(val description:String, val position:Position) {
	def this(description:ExpectingDescription, position:Position) = this(description.value, position)

	def where(condition:ExpectingDescription) = new Expecting(s"$description where ${condition.value}", position)
}

private[stringContextParserCombinator]
object Expecting {
	def apply(description:ExpectingDescription, position:Position) = new Expecting(description, position)
}
