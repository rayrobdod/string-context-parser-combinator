package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set

object TestUtilities {
	val EmptyExpecting = Set.empty[Expecting[StubPosition]]
	def SinglePartInput(str:String, pos:Int) = new Input[Nothing, StubPosition](((str, StubPosition(pos))) :: Nil, Nil, x => x)
	def SingleExpecting(msg:String, pos:Int) = Set(Expecting(ExpectingDescription(msg), StubPosition(pos)))
	def RepeatedExpecting(msg:String, pos:Range) = pos.flatMap(x => SingleExpecting(msg, x)).toSet
}
