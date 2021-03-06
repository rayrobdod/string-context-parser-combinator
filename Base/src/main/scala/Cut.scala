package com.rayrobdod.stringContextParserCombinator

/** Represents whether parsing has passed by a cut */
private[stringContextParserCombinator] sealed trait Cut {
	def |(other:Cut):Cut
	def toBoolean:Boolean
}

/** The instances of `Cut` */
private[stringContextParserCombinator] object Cut {
	object True extends Cut {
		override def |(other:Cut):Cut = this
		override def toBoolean:Boolean = true
		override def toString:String = "Cut.True"
	}
	object False extends Cut {
		override def |(other:Cut):Cut = other
		override def toBoolean:Boolean = false
		override def toString:String = "Cut.False"
	}
}
