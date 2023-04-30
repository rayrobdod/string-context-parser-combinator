package com.rayrobdod.stringContextParserCombinatorExample

import java.time._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator._

package datetime {
	final class MacroImpl(val c:Context {type PrefixType = DateTimeStringContext}) {
		import c.universe._

		private[this] object Name {
			def unapply(input:scala.reflect.api.Universe#Name):Option[String] = Option(input.decodedName.toString)
		}

		private[this] val leafParsers = Interpolator.contextInterpolators(c)
		import leafParsers._
		private[this] val timeLiftables = TimeLiftables(c)
		import timeLiftables._
		private[this] val timeUnliftables = TimeUnliftables(c)
		import timeUnliftables._
		private[this] implicit val thisCToExpr = typeclass.ToExprMapping.forContext(c)

		import TimeParsers.intTwoDigits

		private[this] implicit object sequenced_YearMonth extends typeclass.Sequenced[c.Expr[Year], c.Expr[Month], c.Expr[YearMonth]] {
			def aggregate(left:c.Expr[Year], right:c.Expr[Month]):c.Expr[YearMonth] = {
				import c.universe._
				(left, right) match {
					case (Expr(`unliftYear`(year)), Expr(`unliftMonth`(month))) => c.Expr[YearMonth](liftYearMonth(year.atMonth(month)))
					case (year, month) => c.Expr[YearMonth](q"$year.atMonth($month)")
				}
			}
		}

		private[this] implicit object sequenced_LocalDateTime extends typeclass.Sequenced[c.Expr[LocalDate], c.Expr[LocalTime], c.Expr[LocalDateTime]]{
			def aggregate(left:c.Expr[LocalDate], right:c.Expr[LocalTime]):c.Expr[LocalDateTime] = {
				import c.universe._
				(left, right) match {
					case (Expr(`unliftLocalDate`(date)), Expr(`unliftLocalTime`(time))) => c.Expr[LocalDateTime](liftLocalDateTime(date.atTime(time)))
					case (date, Expr(`unliftLocalTimeParts`(h, m, s, n))) => c.Expr[LocalDateTime](q"$date.atTime($h, $m, $s, $n)")
					case (date, time) => c.Expr[LocalDateTime](q"$date.atTime($time)")
				}
			}
		}

		private val timeParsers = TimeParsers(leafParsers)(
			c.Expr(c.universe.Literal(c.universe.Constant(0))),
			{(ymExpr) => ymExpr match {
				case c.Expr(`unliftYearMonth`(ym)) => {
					intTwoDigits(1, ym.lengthOfMonth).map(day => c.Expr[LocalDate](liftLocalDate(ym.atDay(day))))
				}
				case c.Expr(Apply(Select(_, Name("atMonth")), List(`unliftMonth`(month)))) => {
					intTwoDigits(1, month.maxLength).map({day =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
				case ymExpr => {
					intTwoDigits(1, 31).map({day =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
			}},
			(hour, minute, second, nano) =>
					c.Expr[LocalTime](q"java.time.LocalTime.of($hour, $minute, $second, $nano)")
		)

		private[this] val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

		def interpolate_localDate(args:c.Expr[Any]*):c.Expr[LocalDate] = {
			(timeParsers.localDate andThen End).interpolate(c)(extensionClassName)(args.toList)
		}
		def interpolate_localTime(args:c.Expr[Any]*):c.Expr[LocalTime] = {
			(timeParsers.localTime andThen End).interpolate(c)(extensionClassName)(args.toList)
		}
		def interpolate_localDateTime(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
			(timeParsers.localDateTime andThen End).interpolate(c)(extensionClassName)(args.toList)
		}
	}
}

package object datetime {
	implicit final class DateTimeStringContext(val backing:StringContext) {
		object localdate {
			def apply(args:Any*):LocalDate = macro MacroImpl.interpolate_localDate
		}

		object localtime {
			def apply(args:Any*):LocalTime = macro MacroImpl.interpolate_localTime
		}

		object localdatetime {
			def apply(args:Any*):LocalDateTime = macro MacroImpl.interpolate_localDateTime
		}


		object localdate2 {
			def apply(args:Any*):LocalDate = IdImpl.interpolate_localDate(backing, args:_*)
		}

		object localtime2 {
			def apply(args:Any*):LocalTime = IdImpl.interpolate_localTime(backing, args:_*)
		}

		object localdatetime2 {
			def apply(args:Any*):LocalDateTime = IdImpl.interpolate_localDateTime(backing, args:_*)
		}
	}
}
