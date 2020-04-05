package com.rayrobdod

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * A parser combinator for StringContext handling
 */
package object stringContextParserCombinator {
}

package stringContextParserCombinator {
	// CodePoint extending AnyVal, parameterized methods, and using CodePoint::toString results in a
	// surprisingly high number of NoSuchMethodErrors, at least before 2.12.
	// `java.lang.NoSuchMethodError: 'java.lang.String com.rayrobdod.stringContextParserCombinator.CodePoint$.toString$extension(int)`
	/** A codepoint */
	final case class CodePoint(val value:Int) {
		override def toString:String = new String(Array[Int](value), 0, 1)
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
