package name.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import scala.annotation.nowarn

object ExtractorMacroErrorMessageCompat {
	@nowarn("msg=methodName in method apply is never used")
	def apply(methodName:String, expectingLine:String, contextLine:String, pointerLine:String):String = {
		s"error: $expectingLine\n$contextLine\n$pointerLine\n"
	}
}
