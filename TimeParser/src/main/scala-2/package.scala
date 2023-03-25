package com.rayrobdod.stringContextParserCombinatorExample

import scala.language.experimental.macros
import java.time.{LocalTime, LocalDate, LocalDateTime}

package object datetime {
	implicit final class DateTimeStringContext(val backing:StringContext) {
		object localdate {
			def apply(args:Any*):LocalDate = macro MacroImpl.stringContext_localdate
		}

		object localtime {
			def apply(args:Any*):LocalTime = macro MacroImpl.stringContext_localtime
		}

		object localdatetime {
			def apply(args:Any*):LocalDateTime = macro MacroImpl.stringContext_localdatetime
		}
	}
}
