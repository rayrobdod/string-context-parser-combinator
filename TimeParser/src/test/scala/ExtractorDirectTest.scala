package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import java.time._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinator.ParseException
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class ExtractorDirectTest extends AnyFunSpec {
	describe ("StringContext.localtime2.unapply") {
		it ("can match a whole time") {
			LocalTime.MIDNIGHT match {
				case localtime2"$res" => {
					assertResult(LocalTime.MIDNIGHT)(res)
				}
			}
		}
		it ("a literal midnight string matches the midnight value") {
			LocalTime.MIDNIGHT match {
				 case localtime2"00:00" => // pass
			}
		}
		it ("a literal midnight string does not match a not-midnight value") {
			LocalTime.of(1,2,3) match {
				 case localtime2"00:00" => fail()
				 case _ => // pass
			}
		}
		it ("a literal 01:02:03.4 can match") {
			LocalTime.of(1,2,3,400000000) match {
				 case localtime2"01:02:03.4" => // pass
			}
		}
		it ("Specifying minutes is mandatory") {
			intercept[ParseException](
				LocalTime.MIDNIGHT match {
					 case localtime2"00" =>
				}
			)
		}
		it ("Does not compile if the time has 24 hours") {
			intercept[ParseException](
				LocalTime.MIDNIGHT match {
					 case localtime2"24:00:00" =>
				}
			)
		}
		it ("Does not compile if the time has 60 minutes") {
			intercept[ParseException](
				LocalTime.MIDNIGHT match {
					 case localtime2"00:60:00" =>
				}
			)
		}
	}
	describe ("StringContext.localdate2.unapply") {
		it ("literal string matches corresponding LocalDate") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate2"2001-02-03" => // pass
			}
		}
		it ("literal string does not match a different LocalDate") {
			LocalDate.of(2001, 2, 4) match {
				 case localdate2"2001-02-03" => fail()
				 case _ => // pass
			}
		}
		it ("can extract a YearMonth from value") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate2"${yearMonth}-03" => {
					assertResult(YearMonth.of(2001, 2))(yearMonth)
				}
			}
		}
		it ("can extract a year and a month from value") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate2"${year}-${month}-03" => {
					assertResult(Year.of(2001))(year)
					assertResult(Month.FEBRUARY)(month)
				}
			}
		}
		it ("does not compile if attempting to match month 0") {
			intercept[ParseException](
				LocalDate.of(2001, 2, 3) match {
					case localdate2"2001-00-03" => ()
				}
			)
		}
	}
	describe ("StringContext.localdatetime2.unapply") {
		it ("literal string matches corresponding LocalDateTime") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				 case localdatetime2"2001-02-03T04:05:06" => // pass
			}
		}
		it ("can split into a date and a time") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"${date}T${time}" => {
					assertResult(LocalDate.of(2001, 2, 3))(date)
					assertResult(LocalTime.of(4, 5, 6))(time)
				}
			}
		}
		it ("can extract a date") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"${date}T04:05:06" => {
					assertResult(LocalDate.of(2001, 2, 3))(date)
				}
			}
		}
		it ("can extract a time") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"2001-02-03T${time}" => {
					assertResult(LocalTime.of(4, 5, 6))(time)
				}
			}
		}
		it ("can extract a year") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime2"${year}-02-03T04:05:06" => {
					assertResult(Year.of(2001))(year)
				}
			}
		}
		it ("throws if date-time-splitter is missing the `T`") {
			intercept[ParseException](
				LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
					case localdatetime2"${date}${time}" => {
						assertResult(LocalDate.of(2001, 2, 3))(date)
						assertResult(LocalTime.of(4, 5, 6))(time)
					}
				}
			)
		}
		it ("throws if date-time-splitter is the wrong character") {
			intercept[ParseException](
				LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
					case localdatetime2"${date}Y${time}" => {
						assertResult(LocalDate.of(2001, 2, 3))(date)
						assertResult(LocalTime.of(4, 5, 6))(time)
					}
				}
			)
		}
	}
}
