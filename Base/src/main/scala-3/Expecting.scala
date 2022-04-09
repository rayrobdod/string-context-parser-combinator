package com.rayrobdod.stringContextParserCombinator

/** Represents a textual description of under what conditions a parser would return success */
private[stringContextParserCombinator]
opaque type ExpectingDescription = String

private[stringContextParserCombinator]
inline def ExpectingDescription(backing:String):ExpectingDescription = backing
