package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._

private[datetime] trait TimeUnliftables {
	type Context <: scala.reflect.macros.blackbox.Context
	val ctx:Context
	import ctx.universe.Tree

	implicit object unliftYear extends ctx.universe.Unliftable[Year] {
		def unapply(tree:Tree):Option[Year] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Year")), Name("of")), List(Unliftable.unliftInt(year))) => {
					Some(Year.of(year))
				}
				case _ => None
			}
		}
	}

	implicit object unliftMonth extends ctx.universe.Unliftable[Month] {
		def unapply(tree:Tree):Option[Month] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Month")), Name("of")), List(Unliftable.unliftInt(month))) => {
							Some(Month.of(month))
				}
				case Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("Month")), Name(month)) => {
					Some(Month.valueOf(month))
				}
				case _ => None
			}
		}
	}

	implicit object unliftYearMonth extends ctx.universe.Unliftable[YearMonth] {
		def unapply(tree:Tree):Option[YearMonth] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("YearMonth")), Name("of")), List(Unliftable.unliftInt(year),
						Unliftable.unliftInt(month))) => {
					Some(YearMonth.of(year, month))
				}
				case _ => None
			}
		}
	}

	implicit object unliftLocalDate extends ctx.universe.Unliftable[LocalDate] {
		def unapply(tree:Tree):Option[LocalDate] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("LocalDate")), Name("of")), List(Unliftable.unliftInt(year),
						Unliftable.unliftInt(month), Unliftable.unliftInt(day))) => {
					Some(LocalDate.of(year, month, day))
				}
				case _ => None
			}
		}
	}

	implicit object unliftLocalTime extends ctx.universe.Unliftable[LocalTime] {
		def unapply(tree:Tree):Option[LocalTime] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("LocalTime")), Name("of")), List(Unliftable.unliftInt(hour),
						Unliftable.unliftInt(minute), Unliftable.unliftInt(second), Unliftable.unliftInt(nano))) => {
					Some(LocalTime.of(hour, minute, second, nano))
				}
				case _ => None
			}
		}
	}

	object unliftLocalTimeParts {
		def unapply(tree:Tree):Option[(Int, Int, Int, Int)] = {
			import ctx.universe._
			tree match {
				case Apply(Select(Select(Select(Ident(Name("java")), Name("time")),
						Name("LocalTime")), Name("of")), List(Unliftable.unliftInt(hour),
						Unliftable.unliftInt(minute), Unliftable.unliftInt(second), Unliftable.unliftInt(nano))) => {
					Some((hour, minute, second, nano))
				}
				case _ => None
			}
		}
	}
}

object TimeUnliftables {
	def apply(c:scala.reflect.macros.blackbox.Context):TimeUnliftables {
		type Context = c.type
	} = new TimeUnliftables {
		type Context = c.type
		override val ctx:Context = c
	}
}
