package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import name.rayrobdod.stringContextParserCombinator._

object IdImpl {
	private[this] val leafParsers = Parser.idParsers
	import leafParsers.end
	import leafParsers.ofType
	private[this] val intTwoDigits = (min:Int, max:Int) => TimeParsers.intTwoDigits({(cs: Seq[Char]) => leafParsers.charIn(cs).toInterpolator})(min, max)
	import typeclass.BiEithered.idSymmetric

	private[this] implicit object sequenced_YearMonth extends typeclass.BiSequenced[Any, Year, Month, YearMonth] {
		def aggregate(left:Year, right:Month)(implicit ctx:Any):YearMonth = left.atMonth(right)
		def separate(value:YearMonth)(implicit ctx:Any):(Year, Month) = ((Year.of(value.getYear), value.getMonth))
	}

	private[this] implicit object sequenced_LocalDate extends typeclass.ContraSequenced[Any, YearMonth, Int, LocalDate]{
		def separate(value:LocalDate)(implicit ctx:Any):(YearMonth, Int) = ((YearMonth.of(value.getYear, value.getMonth), value.getDayOfMonth))
	}

	private[this] implicit object sequenced_LocalDateTime extends typeclass.BiSequenced[Any, LocalDate, LocalTime, LocalDateTime]{
		def aggregate(left:LocalDate, right:LocalTime)(implicit ctx:Any):LocalDateTime = left.atTime(right)
		def separate(value:LocalDateTime)(implicit ctx:Any):(LocalDate, LocalTime) = ((value.toLocalDate, value.toLocalTime))
	}

	private[this] implicit val toExprMappingForId: typeclass.ToExprMapping[IdCtx, Id, IdToExpr, ClassTag] =
		typeclass.ToExprMapping.forId

	private val timeParsers = TimeParsers[IdCtx, Id, IdToExpr, ClassTag](leafParsers)(
		(ym, _) => (ofType[DayOfMonth].map((x, _) => x.getValue) <|> intTwoDigits(1, ym.lengthOfMonth)).map((day, _) => ym.atDay(day)),
		(h, m, d, s, _:IdCtx) => java.time.LocalTime.of(h, m, d, s),
		(d, _) => DayOfMonth.of(d),
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
