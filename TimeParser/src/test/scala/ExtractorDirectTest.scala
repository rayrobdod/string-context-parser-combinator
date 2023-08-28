package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest
package extractorDirectTest

import java.time._
import com.rayrobdod.stringContextParserCombinator.ParseException
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class localtime2 extends munit.FunSuite {
	test("can match a whole time") {
		LocalTime.MIDNIGHT match {
			case localtime2"$res" => {
				assertEquals(res, LocalTime.MIDNIGHT)
			}
			case _ => fail("did not match")
		}
	}
	test("a literal midnight string matches the midnight value") {
		LocalTime.MIDNIGHT match {
			case localtime2"00:00" => // pass
			case _ => fail("did not match")
		}
	}
	test("a literal midnight string does not match a not-midnight value") {
		LocalTime.of(1,2,3) match {
			case localtime2"00:00" => fail("Did match")
			case _ => // pass
		}
	}
	test("a literal 01:02:03.4 can match") {
		LocalTime.of(1,2,3,400000000) match {
			case localtime2"01:02:03.4" =>
			case _ => fail("did not match")
		}
	}
	test("Specifying minutes is mandatory") {
		interceptMessage[ParseException]("Expected \":\"\n\t00\n\t  ^")(
			LocalTime.MIDNIGHT match {
				case localtime2"00" =>
				case _ => fail("did not match")
			}
		)
	}
	test("Does not compile if the time has 24 hours") {
		interceptMessage[ParseException]("Expected 00 <= $value <= 23 or OfType(java.time.LocalTime)\n\t24:00:00\n\t^")(
			LocalTime.MIDNIGHT match {
				case localtime2"24:00:00" =>
				case _ => fail("did not match")
			}
		)
	}
	test("Does not compile if the time has 60 minutes") {
		interceptMessage[ParseException]("Expected 00 <= $value <= 59\n\t00:60:00\n\t   ^")(
			LocalTime.MIDNIGHT match {
				case localtime2"00:60:00" =>
				case _ => fail("did not match")
			}
		)
	}
}
final class localdate2 extends munit.FunSuite {
	test("literal string matches corresponding LocalDate") {
		LocalDate.of(2001, 2, 3) match {
			case localdate2"2001-02-03" => // pass
			case _ => fail("did not match")
		}
	}
	test("literal string does not match a different LocalDate") {
		LocalDate.of(2001, 2, 4) match {
			case localdate2"2001-02-03" => fail("Did match")
			case _ => // pass
		}
	}
	test("can extract a YearMonth from value") {
		LocalDate.of(2001, 2, 3) match {
			case localdate2"${yearMonth}-03" => {
				assertEquals(yearMonth, YearMonth.of(2001, 2))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a year and a month from value") {
		LocalDate.of(2001, 2, 3) match {
			case localdate2"${year}-${month}-03" => {
				assertEquals(year, Year.of(2001))
				assertEquals(month, Month.FEBRUARY)
			}
			case _ => fail("did not match")
		}
	}
	test("does not compile if attempting to match month 0") {
		interceptMessage[ParseException]("Expected 01 <= $value <= 12 or OfType(java.time.Month)\n\t2001-00-03\n\t     ^")(
			LocalDate.of(2001, 2, 3) match {
				case localdate2"2001-00-03" => ()
				case _ => fail("did not match")
			}
		)
	}
}
final class localdatetime2 extends munit.FunSuite {
	test("literal string matches corresponding LocalDateTime") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime2"2001-02-03T04:05:06" => // pass
			case _ => fail("did not match")
		}
	}
	test("can split into a date and a time") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime2"${date}T${time}" => {
				assertEquals(date, LocalDate.of(2001, 2, 3))
				assertEquals(time, LocalTime.of(4, 5, 6))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a date") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime2"${date}T04:05:06" => {
				assertEquals(date, LocalDate.of(2001, 2, 3))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a time") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime2"2001-02-03T${time}" => {
				assertEquals(time, LocalTime.of(4, 5, 6))
			}
			case _ => fail("did not match")
		}
	}
	test("can extract a year") {
		LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
			case localdatetime2"${year}-02-03T04:05:06" => {
				assertEquals(year, Year.of(2001))
			}
			case _ => fail("did not match")
		}
	}
	test("throws if date-time-splitter is missing the `T`") {
		interceptMessage[ParseException]("Expected \"T\"\n\t${}${}\n\t   ^")(
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"${date}${time}" => {
					assertEquals(date, LocalDate.of(2001, 2, 3))
					assertEquals(time, LocalTime.of(4, 5, 6))
				}
				case _ => fail("did not match")
			}
		)
	}
	test("throws if date-time-splitter is the wrong character") {
		interceptMessage[ParseException]("Expected \"T\"\n\t${}Y${}\n\t   ^")(
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"${date}Y${time}" => {
					assertEquals(date, LocalDate.of(2001, 2, 3))
					assertEquals(time, LocalTime.of(4, 5, 6))
				}
				case _ => fail("did not match")
			}
		)
	}
}
