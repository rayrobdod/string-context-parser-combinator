package name.rayrobdod.stringContextParserCombinatorExample.datetimeTest
package extractorMacroTest

import java.time._
import name.rayrobdod.stringContextParserCombinatorExample.datetime._

final class localtime extends munit.FunSuite {
	test("can match a whole time") {
		LocalTime.MIDNIGHT match {
			case localtime"$res" => {
				assertEquals(res, LocalTime.MIDNIGHT)
			}
			case _ => fail("did not match")
		}
	}
	test("a literal midnight string matches the midnight value") {
		LocalTime.MIDNIGHT match {
			case localtime"00:00" => // pass
			case _ => fail("did not match")
		}
	}
	test("a literal midnight string does not match a not-midnight value") {
		LocalTime.of(1,2,3) match {
			case localtime"00:00" => fail("did match")
			case _ => // pass
		}
	}
	test("a literal 01:02:03.4 can match") {
		LocalTime.of(1,2,3,400000000) match {
			case localtime"01:02:03.4" => // pass
			case _ => fail("did not match")
		}
	}
	test("Specifying minutes is mandatory") {
		assertNoDiff(
			compileErrors("LocalTime.MIDNIGHT match {\n  case localtime\"00\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localtime",
				"Expected \":\"",
				"  case localtime\"00\" =>",
				"                   ^"
			)
		)
	}
	test("Does not compile if the time has 24 hours") {
		assertNoDiff(
			compileErrors("LocalTime.MIDNIGHT match {\n  case localtime\"24:00:00\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localtime",
				"Expected 00 <= $value <= 23 or OfType(java.time.LocalTime)",
				"  case localtime\"24:00:00\" =>",
				"                 ^"
			)
		)
	}
	test("Does not compile if the time has 60 minutes") {
		assertNoDiff(
			compileErrors("LocalTime.MIDNIGHT match {\n  case localtime\"00:60:00\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localtime",
				"Expected 00 <= $value <= 59",
				"  case localtime\"00:60:00\" =>",
				"                    ^"
			)
		)
	}
}
final class localdate extends munit.FunSuite {
	test("literal string matches corresponding LocalDate") {
		LocalDate.of(2001, 2, 3) match {
			case localdate"2001-02-03" => // pass
			case _ => fail("did not match")
		}
	}
	test("literal string does not match a different LocalDate") {
		LocalDate.of(2001, 2, 4) match {
			case localdate"2001-02-03" => fail("did match")
			case _ => // pass
		}
	}
	test("can extract a YearMonth from value") {
		LocalDate.of(2001, 2, 3) match {
			case localdate"${yearMonth}-03" => {
				assertEquals(yearMonth, YearMonth.of(2001, 2))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a year and a month from value") {
		LocalDate.of(2001, 2, 3) match {
			case localdate"${year}-${month}-03" => {
				assertEquals(year, Year.of(2001))
				assertEquals(month, Month.FEBRUARY)
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a year and a month and a day from value") {
		LocalDate.of(2001, 2, 3) match {
			case localdate"${year}-${month}-${day}" => {
				assertEquals(year, Year.of(2001))
				assertEquals(month, Month.FEBRUARY)
				assertEquals(day, 3)
			}
			case _ => fail("did not match")
		}
	}
	test("can extract y-m-d from a ofEpochDay value") {
		LocalDate.ofEpochDay(10000) match {
			case localdate"${year}-${month}-${day}" => {
				assertEquals(year, Year.of(1997))
				assertEquals(month, Month.MAY)
				assertEquals(day, 19)
			}
			case _ => fail("did not match")
		}
	}
	test("does not compile if attempting to match month 0") {
		assertNoDiff(
			compileErrors("LocalDate.of(2001, 2, 3) match {\n  case localdate\"2001-00-03\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localdate",
				"Expected 01 <= $value <= 12 or OfType(java.time.Month)",
				"  case localdate\"2001-00-03\" =>",
				"                      ^"
			)
		)
	}
}
final class localdatetime extends munit.FunSuite {
	test("literal string matches corresponding LocalDateTime") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime"2001-02-03T04:05:06" => // pass
			case _ => fail("did not match")
		}
	}
	test("can split into a date and a time") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime"${date}T${time}" => {
				assertEquals(date, LocalDate.of(2001, 2, 3))
				assertEquals(time, LocalTime.of(4, 5, 6))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a date") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime"${date}T04:05:06" => {
				assertEquals(date, LocalDate.of(2001, 2, 3))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a time") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime"2001-02-03T${time}" => {
				assertEquals(time, LocalTime.of(4, 5, 6))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a year") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime"${year}-02-03T04:05:06" => {
				assertEquals(year, Year.of(2001))
			}
			case _ => fail("did not match")
		}
	}
	test("does not compile if date-time-splitter is missing the `T`") {
		assertNoDiff(
			compileErrors("LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {\n  case localdatetime\"${date}${time}\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localdatetime",
				"Expected \"T\"",
				"  case localdatetime\"${date}${time}\" =>",
				"                            ^"
			)
		)
	}
	test("does not compile if date-time-splitter is the wrong character") {
		assertNoDiff(
			compileErrors("LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {\n  case localdatetime\"${date}Y${time}\" =>\n}"),
			ExtractorMacroErrorMessageCompat(
				"localdatetime",
				"Expected \"T\"",
				"  case localdatetime\"${date}Y${time}\" =>",
				"                            ^"
			)
		)
	}
}
