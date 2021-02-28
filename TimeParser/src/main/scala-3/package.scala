package com.rayrobdod.stringContextParserCombinatorExample

import java.time.{LocalTime, LocalDate, LocalDateTime}

package object datetime {
	extension (inline sc:scala.StringContext)
		transparent inline def localdate(inline args:Any*):LocalDate =
			${MacroImpl.stringContext_localdate('sc, 'args)}
		transparent inline def localtime(inline args:Any*):LocalTime =
			${MacroImpl.stringContext_localtime('sc, 'args)}
		transparent inline def localdatetime(inline args:Any*):LocalDateTime =
			${MacroImpl.stringContext_localdatetime('sc, 'args)}
}
