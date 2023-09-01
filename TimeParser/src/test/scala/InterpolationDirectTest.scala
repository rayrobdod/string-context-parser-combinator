package name.rayrobdod.stringContextParserCombinatorExample.datetimeTest
package interpolationDirectTest

import java.time._
import name.rayrobdod.stringContextParserCombinatorExample.datetime._

final class localtime2 extends munit.FunSuite {
	test("Accepts a literal midnight") {
		assertEquals(localtime2"00:00", LocalTime.MIDNIGHT)
	}
	test("Accepts a literal max") {
		assertEquals(localtime2"23:59:59.999999999", LocalTime.MAX)
	}
	test("Accepts a literal 01:02:03.4") {
		assertEquals(localtime2"01:02:03.4", LocalTime.of(1,2,3,400000000))
	}
	test("Specifying nanoseconds is optional") {
		assertEquals(localtime2"01:02:03", LocalTime.of(1,2,3))
	}
	test("Specifying seconds is optional") {
		assertEquals(localtime2"01:02", LocalTime.of(1,2))
	}
	test("Specifying minutes is mandatory") {
		interceptMessage[Exception]("Expected \":\"\n\t01\n\t  ^")(localtime2"01")
	}
	test("Rejects a time with 24 hours") {
		interceptMessage[Exception]("Expected 00 <= $value <= 23 or OfType(java.time.LocalTime)\n\t24:00:00\n\t^")(localtime2"24:00:00")
	}
	test("Rejects a time with 60 minutes") {
		interceptMessage[Exception]("Expected 00 <= $value <= 59\n\t00:60:00\n\t   ^")(localtime2"00:60:00")
	}
}
final class localdate2 extends munit.FunSuite {
	test("Accepts a literal 2000-01-01") {
		assertEquals(localdate2"2000-01-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a literal 2000-12-31") {
		assertEquals(localdate2"2000-12-31", LocalDate.of(2000, 12, 31))
	}
	test("Accepts a Year variable") {
		val myyear = Year.of(2000)
		assertEquals(localdate2"${myyear}-01-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a Month variable") {
		val mymonth = Month.of(1)
		assertEquals(localdate2"2000-${mymonth}-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts a YearMonth variable") {
		val myyearmonth = YearMonth.of(2000, 1)
		assertEquals(localdate2"${myyearmonth}-01", LocalDate.of(2000, 1, 1))
	}
	test("Accepts year 999_999_999") {
		assertEquals(localdate2"999999999-01-01", LocalDate.of(999999999, 1, 1))
	}
	test("Rejects year 1_000_000_000") {
		interceptMessage[Exception]("Expected \"-\"\n\t1000000000-01-01\n\t         ^")(localdate2"1000000000-01-01")
	}
	test("Accepts year -999_999_999") {
		assertEquals(localdate2"-999999999-01-01", LocalDate.of(-999999999, 1, 1))
	}
	test("Rejects year -1_000_000_000") {
		interceptMessage[Exception]("Expected \"-\"\n\t-1000000000-01-01\n\t          ^")(localdate2"-1000000000-01-01")
	}
	test("Rejects a month in the year slot") {
		val mymonth = java.time.Month.of(1)
		interceptMessage[Exception]("Expected \"-999999999\"-\"999999999\" or OfType(java.time.LocalDate) or OfType(java.time.Year) or OfType(java.time.YearMonth)\n\t${}-01-01\n\t^")(localdate2"${mymonth}-01-01")
	}
	test("Rejects month 0") {
		interceptMessage[Exception]("Expected 01 <= $value <= 12 or OfType(java.time.Month)\n\t2000-00-01\n\t     ^")(localdate2"2000-00-01")
	}
	test("Rejects month 13") {
		interceptMessage[Exception]("Expected 01 <= $value <= 12 or OfType(java.time.Month)\n\t2000-13-01\n\t     ^")(localdate2"2000-13-01")
	}
	test("Rejects a month missing a digit") {
		interceptMessage[Exception]("Expected 01 <= $value <= 12 or OfType(java.time.Month)\n\t2000-1-01\n\t     ^")(localdate2"2000-1-01")
	}
	test("Rejects day 0") {
		interceptMessage[Exception]("Expected 01 <= $value <= 31\n\t2000-01-00\n\t        ^")(localdate2"2000-01-00")
	}
	test("Rejects day 32") {
		interceptMessage[Exception]("Expected 01 <= $value <= 31\n\t2000-01-32\n\t        ^")(localdate2"2000-01-32")
	}
	test("Rejects February 31st of a known year") {
		interceptMessage[Exception]("Expected 01 <= $value <= 29\n\t2000-02-31\n\t        ^")(localdate2"2000-02-31")
	}
	test("Accepts February 29th of a leap year") {
		assertEquals(localdate2"2004-02-29", LocalDate.of(2004, 2, 29))
	}
	test("Rejects February 29th of a non-leap year") {
		interceptMessage[Exception]("Expected 01 <= $value <= 28\n\t2003-02-29\n\t        ^")(localdate2"2003-02-29")
	}
}
final class localdatetime2 extends munit.FunSuite {
	test("Accepts a literal 2001-02-03T04:05:06") {
		assertEquals(localdatetime2"2001-02-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Date interpolated; Time literal") {
		val date = LocalDate.of(2001, 2, 3)
		assertEquals(localdatetime2"${date}T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Date literal; Time interpolated") {
		val time = LocalTime.of(4, 5, 6)
		assertEquals(localdatetime2"2001-02-03T${time}", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Year interpolated") {
		val year = Year.of(2001)
		assertEquals(localdatetime2"${year}-02-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
	test("Month interpolated") {
		val month = Month.FEBRUARY
		assertEquals(localdatetime2"2001-${month}-03T04:05:06", LocalDateTime.of(2001, 2, 3, 4, 5, 6))
	}
}
