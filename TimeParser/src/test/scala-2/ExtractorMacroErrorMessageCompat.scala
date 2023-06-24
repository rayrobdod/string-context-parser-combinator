package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

object ExtractorMacroErrorMessageCompat {
	def apply(methodName:String, expectingLine:String, contextLine:String, pointerLine:String):String = {
		s"error: $expectingLine\n$contextLine\n$pointerLine\n"
	}
}
