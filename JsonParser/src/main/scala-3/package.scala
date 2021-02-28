package com.rayrobdod.stringContextParserCombinatorExample

import org.json4s.JsonAST.JValue

package object json {
	extension (inline sc:scala.StringContext)
		transparent inline def json(inline args:Any*):JValue =
			${MacroImpl.stringContext_json('sc, 'args)}
}
