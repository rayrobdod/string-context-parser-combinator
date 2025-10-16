package name.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import scala.annotation.unused

object ExtractorMacroErrorMessageCompat {
	def apply(methodName:String, expectingLine:String, contextLine:String, @unused pointerLine:String):String = {
			s"""|error:
				|value $methodName is not a member of StringContext.
				|An extension method was tried, but could not be fully constructed:
				|
				|    ???
				|
				|    failed with:
				|
				|        $expectingLine
				|$contextLine
				|      ^
				|""".stripMargin
	}
}
