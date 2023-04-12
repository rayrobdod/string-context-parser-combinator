package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import com.rayrobdod.stringContextParserCombinator._

object IdImpl {
	private[this] val leafParsers = Interpolator.idInterpolators
	import Interpolator.End
	import TimeParsers.intTwoDigits

	private[this] implicit object sequenced_YearMonth extends typeclass.Sequenced[Year, Month, YearMonth] {
		def aggregate(left:Year, right:Month):YearMonth = left.atMonth(right)
	}

	private[this] implicit object sequenced_LocalDateTime extends typeclass.Sequenced[LocalDate, LocalTime, LocalDateTime]{
		def aggregate(left:LocalDate, right:LocalTime):LocalDateTime = left.atTime(right)
	}

	private val timeParsers = TimeParsers(leafParsers)(
		0,
		(ym) => intTwoDigits(1, ym.lengthOfMonth).map(day => ym.atDay(day)),
		java.time.LocalTime.of _
	)(
		typeclass.ToExprMapping.toExprId,
		classOf[Int],
		classOf[Year],
		classOf[Month],
		classOf[YearMonth],
		classOf[LocalDate],
		classOf[LocalTime],
		implicitly[=:=[Int, Int]],
		implicitly[=:=[Year, Year]],
		implicitly[=:=[Month, Month]],
		sequenced_YearMonth,
		sequenced_LocalDateTime
	)

	def interpolate_localDate(sc:StringContext, args:Any*):LocalDate = {
		(timeParsers.localDate andThen End).interpolate(sc, args.toList)
	}
	def interpolate_localTime(sc:StringContext, args:Any*):LocalTime = {
		(timeParsers.localTime andThen End).interpolate(sc, args.toList)
	}
	def interpolate_localDateTime(sc:StringContext, args:Any*):LocalDateTime = {
		(timeParsers.localDateTime andThen End).interpolate(sc, args.toList)
	}
}
