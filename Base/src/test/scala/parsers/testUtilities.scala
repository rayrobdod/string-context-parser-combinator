package com.rayrobdod.stringContextParserCombinator
package parsers

object TestUtilities {
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing](((str, Position(pos))) :: Nil, Nil, x => x)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(ExpectingDescription(msg), Position(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = pos.flatMap(x => SingleExpecting(msg, x)).toSet
}
