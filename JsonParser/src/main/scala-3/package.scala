package name.rayrobdod.stringContextParserCombinatorExample

import org.json4s.JsonAST.JValue
import name.rayrobdod.stringContextParserCombinator.Unapply

package object json {
	extension (inline sc:scala.StringContext)
		transparent inline def json(inline args:Any*):JValue =
			${MacroImpl.stringContext_json('sc, 'args)}

		transparent inline def json:Unapply[JValue] =
			${MacroImpl.stringContext_json_unapply('sc)}
}
