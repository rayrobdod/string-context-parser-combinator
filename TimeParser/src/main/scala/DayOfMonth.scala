package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import java.time.temporal._

final class DayOfMonth private (private val value: Int) extends TemporalAccessor {
	def getValue: Int = value

	def isValidYearMonth(ym: YearMonth): Boolean = ym.isValidDay(this.value)

	override def isSupported(field: TemporalField): Boolean = {
		field == ChronoField.DAY_OF_MONTH
	}
	override def getLong(field: TemporalField): Long = {
		field match {
			case ChronoField.DAY_OF_MONTH => getValue.longValue
			case _: ChronoField => throw new UnsupportedTemporalTypeException(s"Invalid field $field for DayOfMonth")
			case _ => field.getFrom(this)
		}
	}
	override def toString: String = value.toString
	override def hashCode: Int = value
	override def equals(other: Any): Boolean = {
		other match {
			case other2: DayOfMonth => this.value == other2.value
			case _ => false
		}
	}
}

object DayOfMonth {
	def of(dayOfMonth: Int): DayOfMonth = {
		new DayOfMonth(ChronoField.DAY_OF_MONTH.checkValidIntValue(dayOfMonth.longValue))
	}

	def from(temporal: TemporalAccessor): DayOfMonth = {
		DayOfMonth.of(temporal.get(ChronoField.DAY_OF_MONTH))
	}
}
