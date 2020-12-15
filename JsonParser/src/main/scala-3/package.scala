package com.rayrobdod.stringContextParserCombinatorExample

import scalajson.ast.JValue

package object json {
	extension (inline sc:scala.StringContext)
		inline def json(inline args:Any*):JValue = ${MacroImpl.stringContext_json('sc, 'args)}
}
