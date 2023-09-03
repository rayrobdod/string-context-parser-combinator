package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import name.rayrobdod.stringContextParserCombinator._

object IdImpl {
	private[this] val leafParsers = Parser.idParsers
	import leafParsers.end
	import TimeParsers.intTwoDigits
	import typeclass.BiEithered.idSymmetric

	private[this] implicit object sequenced_YearMonth extends typeclass.BiSequenced[Year, Month, YearMonth] {
		def aggregate(left:Year, right:Month):YearMonth = left.atMonth(right)
		def separate(value:YearMonth):(Year, Month) = ((Year.of(value.getYear), value.getMonth))
	}

	private[this] implicit object sequenced_LocalDate extends typeclass.ContraSequenced[YearMonth, Int, LocalDate]{
		def separate(value:LocalDate):(YearMonth, Int) = ((YearMonth.of(value.getYear, value.getMonth), value.getDayOfMonth))
	}

	private[this] implicit object sequenced_LocalDateTime extends typeclass.BiSequenced[LocalDate, LocalTime, LocalDateTime]{
		def aggregate(left:LocalDate, right:LocalTime):LocalDateTime = left.atTime(right)
		def separate(value:LocalDateTime):(LocalDate, LocalTime) = ((value.toLocalDate, value.toLocalTime))
	}

	private val timeParsers = TimeParsers[Id, IdToExpr, Class](leafParsers)(
		(ym) => intTwoDigits(1, ym.lengthOfMonth).map(day => ym.atDay(day)),
		java.time.LocalTime.of _
	)(
		typeclass.ToExprMapping.forId,
		classOf[Int],
		classOf[Year],
		classOf[Month],
		classOf[YearMonth],
		classOf[LocalDate],
		classOf[LocalTime],
		implicitly[=:=[Int, Int]],
		implicitly[=:=[Year, Year]],
		implicitly[=:=[Month, Month]],
		implicitly[typeclass.BiEithered[Id, Id[Year], Id[Year], Id[Year]]],
		implicitly[typeclass.BiEithered[Id, Id[Month], Id[Month], Id[Month]]],
		implicitly[typeclass.BiEithered[Id, Id[YearMonth], Id[YearMonth], Id[YearMonth]]],
		implicitly[typeclass.BiEithered[Id, Id[LocalDate], Id[LocalDate], Id[LocalDate]]],
		implicitly[typeclass.BiEithered[Id, Id[LocalTime], Id[LocalTime], Id[LocalTime]]],
		sequenced_YearMonth,
		sequenced_LocalDate,
		sequenced_LocalDateTime
	)

	def interpolate_localDate(sc:StringContext, args:Any*):LocalDate = {
		(timeParsers.localDate andThen end).interpolate(sc, args.toList)
	}
	def interpolate_localTime(sc:StringContext, args:Any*):LocalTime = {
		(timeParsers.localTime andThen end).interpolate(sc, args.toList)
	}
	def interpolate_localDateTime(sc:StringContext, args:Any*):LocalDateTime = {
		(timeParsers.localDateTime andThen end).interpolate(sc, args.toList)
	}
	def extract_localDate(sc:StringContext, value:LocalDate):Option[Seq[Any]] = {
		(timeParsers.localDate andThen end).extract(sc, value)
	}
	def extract_localTime(sc:StringContext, value:LocalTime):Option[Seq[Any]] = {
		(timeParsers.localTime andThen end).extract(sc, value)
	}
	def extract_localDateTime(sc:StringContext, value:LocalDateTime):Option[Seq[Any]] = {
		(timeParsers.localDateTime andThen end).extract(sc, value)
	}
}
