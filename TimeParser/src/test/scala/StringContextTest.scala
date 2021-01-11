package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import java.time._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class StringContextTest extends AnyFunSpec {
	describe("StringContext.localtime") {
		it ("Accepts a literal midnight") {
			assertResult(LocalTime.MIDNIGHT)(localtime"00:00")
		}
		it ("Accepts a literal max") {
			assertResult(LocalTime.MAX)(localtime"23:59:59.999999999")
		}
		it ("Accepts a literal 01:02:03.4") {
			assertResult(LocalTime.of(1,2,3,400000000))(localtime"01:02:03.4")
		}
		it ("Specifying nanoseconds is optional") {
			assertResult(LocalTime.of(1,2,3))(localtime"01:02:03")
		}
		it ("Specifying seconds is optional") {
			assertResult(LocalTime.of(1,2))(localtime"01:02")
		}
		it ("Specifying minutes is mandatory") {
			assertDoesNotCompile(""" localtime"01" """)
		}
		it ("Rejects a time with 24 hours") {
			assertDoesNotCompile(""" localtime"24:00:00" """)
		}
		it ("Rejects a time with 60 minutes") {
			assertDoesNotCompile(""" localtime"00:60:00" """)
		}
	}
	describe("StringContext.localdate") {
		it ("Accepts a literal 2000-01-01") {
			assertResult(LocalDate.of(2000, 1, 1))(localdate"2000-01-01")
		}
		it ("Accepts a literal 2000-12-31") {
			assertResult(LocalDate.of(2000, 12, 31))(localdate"2000-12-31")
		}
		it ("Accepts a Year variable") {
			val myyear = Year.of(2000)
			assertResult(LocalDate.of(2000, 1, 1))(localdate"${myyear}-01-01")
		}
		it ("Accepts a Month variable") {
			val mymonth = Month.of(1)
			assertResult(LocalDate.of(2000, 1, 1))(localdate"2000-${mymonth}-01")
		}
		it ("Accepts a YearMonth variable") {
			val myyearmonth = YearMonth.of(2000, 1)
			assertResult(LocalDate.of(2000, 1, 1))(localdate"${myyearmonth}-01")
		}
		it ("Accepts year 999_999_999") {
			assertResult(LocalDate.of(999999999, 1, 1))(localdate"999999999-01-01")
		}
		it ("Rejects year 1_000_000_000") {
			assertDoesNotCompile(""" localdate"1000000000-01-01" """)
		}
		it ("Accepts year -999_999_999") {
			assertResult(LocalDate.of(-999999999, 1, 1))(localdate"-999999999-01-01")
		}
		it ("Rejects year -1_000_000_000") {
			assertDoesNotCompile(""" localdate"-1000000000-01-01" """)
		}
		it ("Rejects a month in the year slot") {
			assertDoesNotCompile("""
				val mymonth = java.time.Month.of(1)
				localdate"${mymonth}-01-01"
			""")
		}
		it ("Rejects month 0") {
			assertDoesNotCompile(""" localdate"2000-00-01" """)
		}
		it ("Rejects month 13") {
			assertDoesNotCompile(""" localdate"2000-13-01" """)
		}
		it ("Rejects a month missing a digit") {
			assertDoesNotCompile(""" localdate"2000-1-01" """)
		}
		it ("Rejects day 0") {
			assertDoesNotCompile(""" localdate"2000-01-00" """)
		}
		it ("Rejects day 32") {
			assertDoesNotCompile(""" localdate"2000-01-32" """)
		}
	}
	describe("StringContext.localdatetime") {
		it ("Accepts a literal 2001-02-03T04:05:06") {
			assertResult(LocalDateTime.of(2001, 2, 3, 4, 5, 6))(localdatetime"2001-02-03T04:05:06")
		}
	}
}
