package com.rayrobdod.stringContextParserCombinatorExample

import scala.language.experimental.macros
import scalajson.ast.JValue

package object json {
	implicit final class JsonStringContext(val backing:StringContext) extends AnyVal {
		def json(args:Any*):JValue = macro MacroImpl.stringContext_json
	}
}
