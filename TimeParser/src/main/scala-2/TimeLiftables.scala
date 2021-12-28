package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.reflect.macros.blackbox.Context

private[datetime] trait TimeLiftables {
	val ctx:Context
	import ctx.universe.Quasiquote

	implicit object liftYear extends ctx.universe.Liftable[Year] {
		def apply(value:Year):ctx.universe.Tree = {
			q"java.time.Year.of(${value.getValue()})"
		}
	}

	implicit object liftMonth extends ctx.universe.Liftable[Month] {
		def apply(value:Month):ctx.universe.Tree = {
			ctx.universe.Select(
				ctx.universe.Select(
					ctx.universe.Select(
						ctx.universe.Ident(ctx.universe.TermName("java")),
						ctx.universe.TermName("time")
					),
					ctx.universe.TermName("Month")
				),
				ctx.universe.TermName(value.name())
			)
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
