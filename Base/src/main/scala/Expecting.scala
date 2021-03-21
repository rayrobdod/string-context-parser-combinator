package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
// opaque type ExpectingDescription = String?
private[stringContextParserCombinator]
final case class ExpectingDescription(value:String) {
	override def toString = value
}
