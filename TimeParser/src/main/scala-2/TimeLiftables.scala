package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._

private[datetime] trait TimeLiftables {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	import ctx.universe.Quasiquote

	implicit object liftYear extends ctx.universe.Liftable[Year] {
		def apply(value:Year):ctx.universe.Tree = {
			q"java.time.Year.of(${value.getValue()})"
		}
	}

	implicit object liftMonth extends ctx.universe.Liftable[Month] {
		def apply(value:Month):ctx.universe.Tree = {
			val monthName = ctx.universe.TermName(value.name())
			q"java.time.Month.${monthName}"
		}
	}

	implicit object liftYearMonth extends ctx.universe.Liftable[YearMonth] {
		def apply(value:YearMonth):ctx.universe.Tree = {
			q"java.time.YearMonth.of(${value.getYear}, ${value.getMonthValue})"
		}
	}

	implicit object liftLocalDate extends ctx.universe.Liftable[LocalDate] {
		def apply(value:LocalDate):ctx.universe.Tree = {
			q"java.time.LocalDate.of(${value.getYear}, ${value.getMonthValue}, ${value.getDayOfMonth})"
		}
	}

	implicit object liftLocalDateTime extends ctx.universe.Liftable[LocalDateTime] {
		def apply(value:LocalDateTime):ctx.universe.Tree = {
			q"""java.time.LocalDateTime.of(${value.getYear}, ${value.getMonthValue}, ${value.getDayOfMonth},
					${value.getHour}, ${value.getMinute}, ${value.getSecond}, ${value.getNano})"""
		}
	}
}

object TimeLiftables {
	def apply(c:scala.reflect.macros.blackbox.Context):TimeLiftables {
		type Context = c.type
	} = new TimeLiftables {
		type Context = c.type
		override val ctx:Context = c
	}
}
