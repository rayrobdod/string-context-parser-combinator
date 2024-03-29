package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.reflect.ClassTag
import name.rayrobdod.stringContextParserCombinator._

object IdImpl {
	private[this] val leafParsers = Parser.idParsers
	import leafParsers.end
	import leafParsers.ofType
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

	private[this] implicit val toExprMappingForId: typeclass.ToExprMapping[Id, IdToExpr, ClassTag] =
		typeclass.ToExprMapping.forId

	private val timeParsers = TimeParsers[Id, IdToExpr, ClassTag](leafParsers)(
		(ym) => (ofType[DayOfMonth].map(_.getValue) <|> intTwoDigits(1, ym.lengthOfMonth)).map(day => ym.atDay(day)),
		java.time.LocalTime.of _,
		DayOfMonth.of _,
	)

	def interpolate_localDate(sc:StringContext, args:Any*):LocalDate = {
		(timeParsers.localDate <~ end).interpolate(sc, args.toList)
	}
	def interpolate_localTime(sc:StringContext, args:Any*):LocalTime = {
		(timeParsers.localTime <~ end).interpolate(sc, args.toList)
	}
	def interpolate_localDateTime(sc:StringContext, args:Any*):LocalDateTime = {
		(timeParsers.localDateTime <~ end).interpolate(sc, args.toList)
	}
	def extract_localDate(sc:StringContext, value:LocalDate):Option[Seq[Any]] = {
		(timeParsers.localDate <~ end).extract(sc, value)
	}
	def extract_localTime(sc:StringContext, value:LocalTime):Option[Seq[Any]] = {
		(timeParsers.localTime <~ end).extract(sc, value)
	}
	def extract_localDateTime(sc:StringContext, value:LocalDateTime):Option[Seq[Any]] = {
		(timeParsers.localDateTime <~ end).extract(sc, value)
	}
}
