package com.rayrobdod.stringContextParserCombinatorExample

import java.time.{LocalTime, LocalDate, LocalDateTime}

package object datetime {
	extension (inline sc:scala.StringContext)
		inline def localdate(inline args:Any*):LocalDate = ${MacroImpl.stringContext_localdate('sc, 'args)}
		inline def localtime(inline args:Any*):LocalTime = ${MacroImpl.stringContext_localtime('sc, 'args)}
		inline def localdatetime(inline args:Any*):LocalDateTime = ${MacroImpl.stringContext_localdatetime('sc, 'args)}
}
