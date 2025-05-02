package name.rayrobdod.stringContextParserCombinatorExample

import java.time._
import scala.collection.immutable.Seq
import scala.reflect.macros.whitebox.Context
import name.rayrobdod.stringContextParserCombinator._

package datetime {
	final class MacroImpl(val c:Context {type PrefixType = DateTimeStringContext}) {
		import c.universe._

		private[this] val leafParsers = Parser.contextParsers(c)
		import leafParsers._
		private[this] val bieithereds = typeclass.BiEithered.forContext(c)
		import bieithereds._
		private[this] val timeLiftables = TimeLiftables(c)
		import timeLiftables._
		private[this] val timeUnliftables = TimeUnliftables(c)
		import timeUnliftables._
		private[this] implicit val thisCToExpr:typeclass.ToExprMapping[c.type, c.Expr, c.universe.Liftable, c.TypeTag] = typeclass.ToExprMapping.forContext(c)

		import TimeParsers.intTwoDigits

		private[this] implicit object sequenced_YearMonth extends typeclass.BiSequenced[c.type, c.Expr[Year], c.Expr[Month], c.Expr[YearMonth]] {
			def aggregate(left:c.Expr[Year], right:c.Expr[Month])(implicit ctx:c.type):c.Expr[YearMonth] = {
				import c.universe._
				(left, right) match {
					case (Expr(`unliftYear`(year)), Expr(`unliftMonth`(month))) => c.Expr[YearMonth](liftYearMonth(year.atMonth(month)))
					case (year, month) => c.Expr[YearMonth](q"$year.atMonth($month)")
				}
			}
			def separate(value:c.Expr[YearMonth])(implicit ctx:c.type):(c.Expr[Year], c.Expr[Month]) = {
				import c.universe._
				(c.Expr[Year](q"Year.of($value.getYear)"), c.Expr[Month](q"$value.getMonth"))
			}
		}

		private[this] implicit object sequenced_LocalDate extends typeclass.ContraSequenced[c.type, Expr[YearMonth], Expr[Int], Expr[LocalDate]] {
			def separate(value:Expr[LocalDate])(implicit ctx:c.type):(Expr[YearMonth], Expr[Int]) = {
				(c.Expr[YearMonth](q"YearMonth.of($value.getYear, $value.getMonth)"), c.Expr[Int](q"$value.getDayOfMonth"))
			}
		}

		private[this] implicit object sequenced_LocalDateTime extends typeclass.BiSequenced[c.type, c.Expr[LocalDate], c.Expr[LocalTime], c.Expr[LocalDateTime]]{
			def aggregate(left:c.Expr[LocalDate], right:c.Expr[LocalTime])(implicit ctx:c.type):c.Expr[LocalDateTime] = {
				import c.universe._
				(left, right) match {
					case (Expr(`unliftLocalDate`(date)), Expr(`unliftLocalTime`(time))) => c.Expr[LocalDateTime](liftLocalDateTime(date.atTime(time)))
					case (date, Expr(`unliftLocalTimeParts`(h, m, s, n))) => c.Expr[LocalDateTime](q"$date.atTime($h, $m, $s, $n)")
					case (date, time) => c.Expr[LocalDateTime](q"$date.atTime($time)")
				}
			}
			def separate(value:c.Expr[LocalDateTime])(implicit ctx:c.type):(c.Expr[LocalDate], c.Expr[LocalTime]) = {
				import c.universe._
				(c.Expr[LocalDate](q"$value.toLocalDate"), c.Expr[LocalTime](q"$value.toLocalTime"))
			}
		}

		private val charIn:(Seq[Char]) => Interpolator[Char] = (chars: Seq[Char]) => Interpolator.contextInterpolators(c).charIn(chars)

		private def dayOfMonth(max: Int): Interpolator[c.Expr[Int]] = {
			ofType[DayOfMonth].map((d, _:c.type) => c.Expr[Int](q"$d.getValue")) <|> intTwoDigits(charIn)(1, max).mapToExpr
		}

		private val timeParsers = TimeParsers(leafParsers)(
			{(ymExpr, _:c.type) => ymExpr match {
				case c.Expr(`unliftYearMonth`(ym)) => {
					ofType[DayOfMonth].map((d, _:c.type) => c.Expr[LocalDate](q"LocalDate.of(${ym.getYear}, ${ym.getMonth}, ${d}.getValue)")) <|>
						intTwoDigits(charIn)(1, ym.lengthOfMonth).map((day, _:c.type) => c.Expr[LocalDate](liftLocalDate(ym.atDay(day))))
				}
				case c.Expr(Apply(Select(_, Name("atMonth")), List(`unliftMonth`(month)))) => {
					dayOfMonth(month.maxLength).map({(day, _:c.type) =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
				case ymExpr => {
					dayOfMonth(31).map({(day, _:c.type) =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
			}},
			(hour, minute, second, nano, _:c.type) =>
					c.Expr[LocalTime](q"java.time.LocalTime.of($hour, $minute, $second, $nano)"),
			(value, _:c.type) => c.Expr[DayOfMonth](q"DayOfMonth.of($value)"),
		)

		private[this] val extensionClassName = "name.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

		def interpolate_localDate(args:c.Expr[Any]*):c.Expr[LocalDate] = {
			(timeParsers.localDate <~ end).interpolate(c)(extensionClassName)(args.toList)
		}
		def interpolate_localTime(args:c.Expr[Any]*):c.Expr[LocalTime] = {
			(timeParsers.localTime <~ end).interpolate(c)(extensionClassName)(args.toList)
		}
		def interpolate_localDateTime(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
			(timeParsers.localDateTime <~ end).interpolate(c)(extensionClassName)(args.toList)
		}

		def extractor_localDate(value:c.Expr[LocalDate]):c.Expr[Any] = {
			(timeParsers.localDate <~ end).extractor(c)(extensionClassName)(value)
		}
		def extractor_localTime(value:c.Expr[LocalTime]):c.Expr[Any] = {
			(timeParsers.localTime <~ end).extractor(c)(extensionClassName)(value)
		}
		def extractor_localDateTime(value:c.Expr[LocalDateTime]):c.Expr[Any] = {
			(timeParsers.localDateTime <~ end).extractor(c)(extensionClassName)(value)
		}
	}
}

package object datetime {
	implicit final class DateTimeStringContext(val backing:StringContext) {
		object localdate {
			def apply(args:Any*):LocalDate = macro MacroImpl.interpolate_localDate
			def unapply(value:LocalDate):Any = macro MacroImpl.extractor_localDate
		}

		object localtime {
			def apply(args:Any*):LocalTime = macro MacroImpl.interpolate_localTime
			def unapply(value:LocalTime):Any = macro MacroImpl.extractor_localTime
		}

		object localdatetime {
			def apply(args:Any*):LocalDateTime = macro MacroImpl.interpolate_localDateTime
			def unapply(value:LocalDateTime):Any = macro MacroImpl.extractor_localDateTime
		}


		object localdate2 {
			def apply(args:Any*):LocalDate = IdImpl.interpolate_localDate(backing, args:_*)
			def unapplySeq(value:LocalDate):Option[Seq[Any]] = IdImpl.extract_localDate(backing, value)
		}

		object localtime2 {
			def apply(args:Any*):LocalTime = IdImpl.interpolate_localTime(backing, args:_*)
			def unapplySeq(value:LocalTime):Option[Seq[Any]] = IdImpl.extract_localTime(backing, value)
		}

		object localdatetime2 {
			def apply(args:Any*):LocalDateTime = IdImpl.interpolate_localDateTime(backing, args:_*)
			def unapplySeq(value:LocalDateTime):Option[Seq[Any]] = IdImpl.extract_localDateTime(backing, value)
		}
	}
}
