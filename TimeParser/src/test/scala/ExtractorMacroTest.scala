package com.rayrobdod.stringContextParserCombinatorExample.datetimeTest

import java.time._
import org.scalatest.funspec.AnyFunSpec
import com.rayrobdod.stringContextParserCombinatorExample.datetime._

final class ExtractorMacroTest extends AnyFunSpec {
	describe ("StringContext.localtime.unapply") {
		it ("can match a whole time") {
			LocalTime.MIDNIGHT match {
				case localtime"$res" => {
					assertResult(LocalTime.MIDNIGHT)(res)
				}
			}
		}
		it ("a literal midnight string matches the midnight value") {
			LocalTime.MIDNIGHT match {
				 case localtime"00:00" => // pass
			}
		}
		it ("a literal midnight string does not match a not-midnight value") {
			LocalTime.of(1,2,3) match {
				 case localtime"00:00" => fail()
				 case _ => // pass
			}
		}
		it ("a literal 01:02:03.4 can match") {
			LocalTime.of(1,2,3,400000000) match {
				 case localtime"01:02:03.4" => // pass
			}
		}
		it ("Specifying minutes is mandatory") {
			assertDoesNotCompile(
				"""
				LocalTime.MIDNIGHT match {
					 case localtime"00" =>
				}
				"""
			)
		}
		it ("Does not compile if the time has 24 hours") {
			assertDoesNotCompile(
				"""
				LocalTime.MIDNIGHT match {
					 case localtime"24:00:00" =>
				}
				"""
			)
		}
		it ("Does not compile if the time has 60 minutes") {
			assertDoesNotCompile(
				"""
				LocalTime.MIDNIGHT match {
					 case localtime"00:60:00" =>
				}
				"""
			)
		}
	}
	describe ("StringContext.localtime.unapply") {
		it ("literal string matches corresponding LocalDate") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate"2001-02-03" => // pass
			}
		}
		it ("literal string does not match a different LocalDate") {
			LocalDate.of(2001, 2, 4) match {
				 case localdate"2001-02-03" => fail()
				 case _ => // pass
			}
		}
		it ("can extract a YearMonth from value") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate"${yearMonth}-03" => {
					assertResult(YearMonth.of(2001, 2))(yearMonth)
				}
			}
		}
		it ("can extract a year and a month from value") {
			LocalDate.of(2001, 2, 3) match {
				 case localdate"${year}-${month}-03" => {
					assertResult(Year.of(2001))(year)
					assertResult(Month.FEBRUARY)(month)
				}
			}
		}
		it ("does not compile if attempting to match month 0") {
			assertDoesNotCompile(
				"""
					LocalDate.of(2001, 2, 3) match {
						case localdate"2001-00-03" => ()
					}
				"""
			)
		}
	}
	describe ("StringContext.localdatetime.unapply") {
		it ("literal string matches corresponding LocalDateTime") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				 case localdatetime"2001-02-03T04:05:06" => // pass
			}
		}
		it ("can split into a date and a time") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime"${date}T${time}" => {
					assertResult(LocalDate.of(2001, 2, 3))(date)
					assertResult(LocalTime.of(4, 5, 6))(time)
				}
			}
		}
		it ("can extract a date") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime"${date}T04:05:06" => {
					assertResult(LocalDate.of(2001, 2, 3))(date)
				}
			}
		}
		it ("can extract a time") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime"2001-02-03T${time}" => {
					assertResult(LocalTime.of(4, 5, 6))(time)
				}
			}
		}
		it ("can extract a year") {
			LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
				case localdatetime"${year}-02-03T04:05:06" => {
					assertResult(Year.of(2001))(year)
				}
			}
		}
		it ("does not compile if date-time-splitter is missing the `T`") {
			assertDoesNotCompile(
				"""
					LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
						case localdatetime"${date}${time}" => {
							assertResult(LocalDate.of(2001, 2, 3))(date)
							assertResult(LocalTime.of(4, 5, 6))(time)
						}
					}
				"""
			)
		}
		it ("does not compile if date-time-splitter is the wrong character") {
			assertDoesNotCompile(
				"""
					LocalDateTime.of(2001, 2, 3, 4, 5, 6) match {
						case localdatetime"${date}Y${time}" => {
							assertResult(LocalDate.of(2001, 2, 3))(date)
							assertResult(LocalTime.of(4, 5, 6))(time)
						}
					}
				"""
			)
		}
	}
}
