package com.rayrobdod

import scala.reflect.macros.blackbox.Context

/**
 * A parser combinator for StringContext handling
 */
package object stringContextParserCombinator {
}

package stringContextParserCombinator {
	/** A codepoint */
	final case class CodePoint(val value:Int) extends AnyVal {
		override def toString:String = new String(new Array[Int](value), 0, 1)
	}

	/** A position's point - divorced from the position's context */
	final case class PositionPoint(val value:Int) extends AnyVal {
		def cast(c:Context):c.Position = c.enclosingPosition.withPoint(value)
		def +(x:Int):PositionPoint = PositionPoint(this.value + x)
		def >(rhs:PositionPoint):Boolean = this.value > rhs.value
	}
	object PositionPoint {
		def apply(x:scala.reflect.api.Position):PositionPoint = new PositionPoint(x.point)
	}
}
