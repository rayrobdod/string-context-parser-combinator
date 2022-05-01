package com.rayrobdod.stringContextParserCombinator

final case class StubPosition(point:Int) {
}

object StubPosition {
	implicit object given_StubPosition_Position extends Position[StubPosition] {
		def offset(pos:StubPosition, offset:Int):StubPosition = new StubPosition(pos.point + offset)
	}
}
