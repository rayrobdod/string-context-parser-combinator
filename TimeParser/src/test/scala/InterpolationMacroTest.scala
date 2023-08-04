package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest
package interpolationMacroTest

import java.time._
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class localtime extends munit.FunSuite {
	test("Accepts a literal midnight") {
		assertEquals(localtime"00:00", LocalTime.MIDNIGHT)
	}
	test("Accepts a literal max") {
		assertEquals(localtime"23:59:59.999999999", LocalTime.MAX)
	}
	test("Accepts a literal 01:02:03.4") {
		assertEquals(localtime"01:02:03.4", LocalTime.of(1,2,3,400000000))
	}
	test("Specifying nanoseconds is optional") {
		assertEquals(localtime"01:02:03", LocalTime.of(1,2,3))
	}
	test("Specifying seconds is optional") {
		assertEquals(localtime"01:02", LocalTime.of(1,2))
	}
	test("Specifying minutes is mandatory") {
		assertNoDiff(
			compileErrors("""localtime"01""""),
			"""|error: Expected ":"
				|localtime"01"
				|            ^
				|""".stripMargin
		)
	}
	test("Rejects a time with 24 hours") {
		assertNoDiff(
			compileErrors("""localtime"24:00:00""""),
			"""|error: Expected 00 <= $value <= 23 or OfType(java.time.LocalTime)
				|localtime"24:00:00"
				|          ^
				|""".stripMargin
		)
	}
	test("Rejects a time with 60 minutes") {
		assertNoDiff(
			compileErrors("""localtime"00:60:00""""),
			"""|error: Expected 00 <= $value <= 59
				|localtime"00:60:00"
				|             ^
				|""".stripMargin
		)
	}
}
final class localdate extends munit.FunSuite {
	test("Accepts a literal 2000-01-01") {
		assertEquals(localdate"2000-01-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a literal 2000-12-31") {
		assertEquals(localdate"2000-12-31", LocalDate.of(2000, 12, 31))
	}
	test("Accepts a Year variable") {
		val myyear = Year.of(2000)
		assertEquals(localdate"${myyear}-01-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a Month variable") {
		val mymonth = Month.of(1)
		assertEquals(localdate"2000-${mymonth}-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a YearMonth variable") {
		val myyearmonth = YearMonth.of(2000, 1)
		assertEquals(localdate"${myyearmonth}-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts year 999_999_999") {
		assertEquals(localdate"999999999-01-01", LocalDate.of(999999999, 1, 1))
	}
	test("Rejects year 1_000_000_000") {
		assertNoDiff(
			compileErrors("""localdate"1000000000-01-01""""),
			"""|error: Expected "-"
				|localdate"1000000000-01-01"
				|                   ^
				|""".stripMargin
		)
	}
	test("Accepts year -999_999_999") {
		assertEquals(localdate"-999999999-01-01", LocalDate.of(-999999999, 1, 1))
	}
	test("Rejects year -1_000_000_000") {
		assertNoDiff(
			compileErrors("""localdate"-1000000000-01-01""""),
			"""|error: Expected "-"
				|localdate"-1000000000-01-01"
				|                    ^
				|""".stripMargin
		)
	}
	test("Rejects a month in the year slot") {
		assertNoDiff(
			compileErrors("val mymonth = java.time.Month.of(1)\nlocaldate\"${mymonth}-01-01\""),
			"""|error: Expected "-999999999"-"999999999" or OfType(java.time.LocalDate) or OfType(java.time.Year) or OfType(java.time.YearMonth)
				|localdate"${mymonth}-01-01"
				|            ^
				|""".stripMargin
		)
	}
	test("Rejects month 0") {
		assertNoDiff(
			compileErrors("""localdate"2000-00-01""""),
			"""|error: Expected 01 <= $value <= 12 or OfType(java.time.Month)
				|localdate"2000-00-01"
				|               ^
				|""".stripMargin
		)
	}
	test("Rejects month 13") {
		assertNoDiff(
			compileErrors("""localdate"2000-13-01""""),
			"""|error: Expected 01 <= $value <= 12 or OfType(java.time.Month)
				|localdate"2000-13-01"
				|               ^
				|""".stripMargin
		)
	}
	test("Rejects a month missing a digit") {
		assertNoDiff(
			compileErrors("""localdate"2000-1-01""""),
			"""|error: Expected 01 <= $value <= 12 or OfType(java.time.Month)
				|localdate"2000-1-01"
				|               ^
				|""".stripMargin
		)
	}
	test("Rejects day 0") {
		assertNoDiff(
			compileErrors("""localdate"2000-01-00""""),
			"""|error: Expected 01 <= $value <= 31
				|localdate"2000-01-00"
				|                  ^
				|""".stripMargin
		)
	}
	test("Rejects day 32") {
		assertNoDiff(
			compileErrors("""localdate"2000-01-32""""),
			"""|error: Expected 01 <= $value <= 31
				|localdate"2000-01-32"
				|                  ^
				|""".stripMargin
		)
	}
	test("Rejects February 31st of a known year") {
		assertNoDiff(
			compileErrors("""localdate"2000-02-31""""),
			"""|error: Expected 01 <= $value <= 29
				|localdate"2000-02-31"
				|                  ^
				|""".stripMargin
		)
	}
	test("Rejects February 31st of an unknown year") {
		assertNoDiff(
			compileErrors("val year = Year.of(2000);\nlocaldate\"${year}-02-31\""),
			"""|error: Expected 01 <= $value <= 29
				|localdate"${year}-02-31"
				|                     ^
				|""".stripMargin
		)
	}
	test("Accepts the 31st of an unknown month") {
		val month = Month.FEBRUARY
		intercept[DateTimeException]{
			localdate"2000-$month-31"
		}
	}
	test("Accepts February 29th of a leap year") {
		assertEquals(localdate"2004-02-29", LocalDate.of(2004, 2, 29))
	}
	test("Rejects February 29th of a non-leap year") {
		assertNoDiff(
			compileErrors("""localdate"2003-02-29""""),
			"""|error: Expected 01 <= $value <= 28
				|localdate"2003-02-29"
				|                  ^
				|""".stripMargin
		)
	}
}
final class localdatetime extends munit.FunSuite {
	test("Accepts a literal 2001-02-03T04:05:06") {
		assertEquals(localdatetime"2001-02-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Date interpolated; Time literal") {
		val date = LocalDate.of(2001, 2, 3)
		assertEquals(localdatetime"${date}T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Date literal; Time interpolated") {
		val time = LocalTime.of(4, 5, 6)
		assertEquals(localdatetime"2001-02-03T${time}", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Year interpolated") {
		val year = Year.of(2001)
		assertEquals(localdatetime"${year}-02-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Month interpolated") {
		val month = Month.FEBRUARY
		assertEquals(localdatetime"2001-${month}-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
}
