package com.rayrobdod.stringContextParserCombinator
package parsers

object TestUtilities {
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(ExpectingDescription(msg), Position(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = pos.flatMap(x => SingleExpecting(msg, x)).toSet
}