package com.rayrobdod.stringContextParserCombinator

// CodePoint extending AnyVal, parameterized methods, and using CodePoint::toString results in a
// surprisingly high number of NoSuchMethodErrors, at least before 2.12.
// `java.lang.NoSuchMethodError: 'java.lang.String com.rayrobdod.stringContextParserCombinator.CodePoint$.toString$extension(int)`
/** A unicode codepoint */
final case class CodePoint(val value:Int) {
	override def toString:String = new String(Array[Int](value), 0, 1)
}
