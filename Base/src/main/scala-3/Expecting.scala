package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
private[stringContextParserCombinator]
opaque type ExpectingDescription = String

private[stringContextParserCombinator]
inline def ExpectingDescription(backing:String):ExpectingDescription = backing

extension (description:ExpectingDescription)
	private[stringContextParserCombinator]
	def where(condition:ExpectingDescription):ExpectingDescription = s"$description where $condition"

private[stringContextParserCombinator]
final case class Expecting[Pos](val description:ExpectingDescription, val position:Pos) {
	def where(condition:ExpectingDescription) = new Expecting(description.where(condition), position)
}
