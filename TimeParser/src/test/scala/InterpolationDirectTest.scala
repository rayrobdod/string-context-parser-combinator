package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import java.time._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class InterpolationDirectTest extends AnyFunSpec {
	describe("StringContext.localtime2") {
		it ("Accepts a literal midnight") {
			assertResult(LocalTime.MIDNIGHT)(localtime2"00:00")
		}
		it ("Accepts a literal max") {
			assertResult(LocalTime.MAX)(localtime2"23:59:59.999999999")
		}
		it ("Accepts a literal 01:02:03.4") {
			assertResult(LocalTime.of(1,2,3,400000000))(localtime2"01:02:03.4")
		}
		it ("Specifying nanoseconds is optional") {
			assertResult(LocalTime.of(1,2,3))(localtime2"01:02:03")
		}
		it ("Specifying seconds is optional") {
			assertResult(LocalTime.of(1,2))(localtime2"01:02")
		}
		it ("Specifying minutes is mandatory") {
			intercept[Exception](localtime2"01")
		}
		it ("Rejects a time with 24 hours") {
			intercept[Exception](localtime2"24:00:00")
		}
		it ("Rejects a time with 60 minutes") {
			intercept[Exception](localtime2"00:60:00")
		}
	}
	describe("StringContext.localdate2") {
		it ("Accepts a literal 2000-01-01") {
			assertResult(LocalDate.of(2000, 1, 1))(localdate2"2000-01-01")
		}
		it ("Accepts a literal 2000-12-31") {
			assertResult(LocalDate.of(2000, 12, 31))(localdate2"2000-12-31")
		}
		it ("Accepts a Year variable") {
			val myyear = Year.of(2000)
			assertResult(LocalDate.of(2000, 1, 1))(localdate2"${myyear}-01-01")
		}
		it ("Accepts a Month variable") {
			val mymonth = Month.of(1)
			assertResult(LocalDate.of(2000, 1, 1))(localdate2"2000-${mymonth}-01")
		}
		it ("Accepts a YearMonth variable") {
			val myyearmonth = YearMonth.of(2000, 1)
			assertResult(LocalDate.of(2000, 1, 1))(localdate2"${myyearmonth}-01")
		}
		it ("Accepts year 999_999_999") {
			assertResult(LocalDate.of(999999999, 1, 1))(localdate2"999999999-01-01")
		}
		it ("Rejects year 1_000_000_000") {
			intercept[Exception](localdate2"1000000000-01-01")
		}
		it ("Accepts year -999_999_999") {
			assertResult(LocalDate.of(-999999999, 1, 1))(localdate2"-999999999-01-01")
		}
		it ("Rejects year -1_000_000_000") {
			intercept[Exception](localdate2"-1000000000-01-01")
		}
		it ("Rejects a month in the year slot") {
			val mymonth = java.time.Month.of(1)
			intercept[Exception](localdate2"${mymonth}-01-01")
		}
		it ("Rejects month 0") {
			intercept[Exception](localdate2"2000-00-01")
		}
		it ("Rejects month 13") {
			intercept[Exception](localdate2"2000-13-01")
		}
		it ("Rejects a month missing a digit") {
			intercept[Exception](localdate2"2000-1-01")
		}
		it ("Rejects day 0") {
			intercept[Exception](localdate2"2000-01-00")
		}
		it ("Rejects day 32") {
			intercept[Exception](localdate2"2000-01-32")
		}
		it ("Rejects February 31st of a known year") {
			intercept[Exception](localdate2"2000-02-31")
		}
		it ("Accepts February 29th of a leap year") {
			assertResult(LocalDate.of(2004, 2, 29))(localdate2"2004-02-29")
		}
		it ("Rejects February 29th of a non-leap year") {
			intercept[Exception](localdate2"2003-02-29")
		}
	}
	describe("StringContext.localdatetime2") {
		it ("Accepts a literal 2001-02-03T04:05:06") {
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime2"2001-02-03T04:05:06")
		}
		it ("Date interpolated; Time literal") {
			val date = LocalDate.of(2001, 2, 3)
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime2"${date}T04:05:06")
		}
		it ("Date literal; Time interpolated") {
			val time = LocalTime.of(4, 5, 6)
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime2"2001-02-03T${time}")
		}
		it ("Year interpolated") {
			val year = Year.of(2001)
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime2"${year}-02-03T04:05:06")
		}
		it ("Month interpolated") {
			val month = Month.FEBRUARY
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime2"2001-${month}-03T04:05:06")
		}
	}
}
