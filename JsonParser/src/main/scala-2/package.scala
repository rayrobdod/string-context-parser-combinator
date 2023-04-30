package com.rayrobdod.stringContextParserCombinatorExample

import scala.language.experimental.macros
import org.json4s.JsonAST.JValue

package object json {
	implicit final class JsonStringContext(val backing:StringContext) {
		object json {
			def apply(args:Any*):JValue = macro MacroImpl.stringContext_json
			def unapply(value:JValue):Any = macro MacroImpl.stringContext_json_unapply
		}
	}
}
