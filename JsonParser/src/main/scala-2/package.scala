package name.rayrobdod.stringContextParserCombinatorExample

import org.json4s.JsonAST.JValue
import org.json4s._

package object json {
	implicit final class JsonStringContext(val backing:StringContext) {
		object json {
			def apply(args:Any*):JValue = macro MacroImpl.stringContext_json
			def unapply(value:JValue):Any = macro MacroImpl.stringContext_json_unapply
		}
	}

	def jnumber2bigdecimal(n: JValue with JNumber): BigDecimal = {
		n match {
			case JDecimal(bd) => bd
			case JDouble(d) => d
			case JInt(bi) => BigDecimal(bi)
			case JLong(i) => i
		}
	}
}
