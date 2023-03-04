package com.rayrobdod.stringContextParserCombinator

final case class StubPosition(point:Int) {
}

object StubPosition {
	implicit object given_StubPosition_Position extends Position[StubPosition] {
		def offset(pos:StubPosition, offset:Int):StubPosition = new StubPosition(pos.point + offset)
	}
	implicit val given_StubPosition_Ordering: Ordering[StubPosition] = Ordering.by(_.point)
}
