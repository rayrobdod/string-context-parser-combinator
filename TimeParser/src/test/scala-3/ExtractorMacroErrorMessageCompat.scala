package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

object ExtractorMacroErrorMessageCompat {
	def apply(methodName:String, expectingLine:String, contextLine:String, pointerLine:String):String = {
			s"""|error:
				|value $methodName is not a member of StringContext.
				|An extension method was tried, but could not be fully constructed:
				|
				|    <empty>
				|
				|    failed with:
				|
				|        $expectingLine
				|$contextLine
				|      ^
				|""".stripMargin
	}
}
