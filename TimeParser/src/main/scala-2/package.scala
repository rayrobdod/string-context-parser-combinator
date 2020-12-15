package com.rayrobdod.stringContextParserCombinatorExample

import scala.language.experimental.macros
import java.time.{LocalTime, LocalDate, LocalDateTime}

package object datetime {
	implicit final class DateTimeStringContext(val backing:StringContext) extends AnyVal {
		def localdate(args:Any*):LocalDate = macro MacroImpl.stringContext_localdate
		def localtime(args:Any*):LocalTime = macro MacroImpl.stringContext_localtime
		def localdatetime(args:Any*):LocalDateTime = macro MacroImpl.stringContext_localdatetime
	}
}
