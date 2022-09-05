package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.quoted._

/**
 * Contains [[scala.quoted.ToExpr]] and [[scala.quoted.FromExpr]] givens for select java.time types
 */
private[datetime] object ExprConversions {
	given ToExpr[Year] with {
		def apply(x:Year)(using Quotes):Expr[Year] = {
			'{Year.of(${Expr(x.getValue)})}
		}
	}

	given FromExpr[Year] with {
		def unapply(x:Expr[Year])(using Quotes):Option[Year] = x match {
			case '{Year.of(${Expr(index)})} => Some(Year.of(index))
			case _ => None
		}
	}

	given ToExpr[Month] with {
		def apply(x:Month)(using Quotes):Expr[Month] = {
			// '{Month.of(${Expr(x.getValue)})}
			import scala.quoted.quotes.reflect._
			val _root = defn.RootPackage
			val _java = _root.fieldMember("java")
			val _time = _java.fieldMember("time")
			val _month = _time.fieldMember("Month")
			val _instance = _month.fieldMember(x.name)

			Ref(_root).select(_java).select(_time).select(_month).select(_instance).asExprOf[Month]
		}
	}

	given FromExpr[Month] with {
		def unapply(x:Expr[Month])(using Quotes):Option[Month] = {
			import scala.quoted.quotes.reflect._
			x match {
				case '{Month.of(${Expr(index)})} => Some(Month.of(index))
				case _ => x.asTerm match {
					case Select(Select(Select(Select(Ident("_root_"), "java"), "time"), "Month"), monthName) => {
						Some(Month.valueOf(monthName))
					}
					case _ => None
				}
			}
		}
	}

	given FromExpr[YearMonth] with {
		def unapply(x:Expr[YearMonth])(using Quotes):Option[YearMonth] = x match {
			case '{YearMonth.of(${Expr(y)}:Int, ${Expr(m)}:Int)} => Some(YearMonth.of(y, m))
			case '{YearMonth.of(${Expr(y)}:Int, ${Expr(m)}:Month)} => Some(YearMonth.of(y, m))
			case _ => None
		}
	}

	given ToExpr[LocalDate] with {
		def apply(x:LocalDate)(using Quotes):Expr[LocalDate] = {
			'{LocalDate.of(${Expr(x.getYear)}, ${Expr(x.getMonthValue)}, ${Expr(x.getDayOfMonth)})}
		}
	}

	given FromExpr[LocalDate] with {
		def unapply(x:Expr[LocalDate])(using Quotes):Option[LocalDate] = x match {
			case '{LocalDate.of(${Expr(y)}:Int, ${Expr(m)}:Int, ${Expr(d)}:Int)} => Some(LocalDate.of(y, m, d))
			case '{LocalDate.of(${Expr(y)}:Int, ${Expr(m)}:Month, ${Expr(d)}:Int)} => Some(LocalDate.of(y, m, d))
			case '{LocalDate.ofEpochDay(${Expr(x)})} => Some(LocalDate.ofEpochDay(x))
			case _ => None
		}
	}

	given ToExpr[LocalTime] with {
		def apply(x:LocalTime)(using Quotes):Expr[LocalTime] = {
			'{LocalTime.of(
				${Expr(x.getHour())},
				${Expr(x.getMinute())},
				${Expr(x.getSecond())},
				${Expr(x.getNano())},
			)}
		}
	}

	given FromExpr[LocalTime] with {
		def unapply(x:Expr[LocalTime])(using Quotes):Option[LocalTime] = x match {
			case '{LocalTime.of(${Expr(h)}, ${Expr(m)}, ${Expr(s)}, ${Expr(n)})} => Some(LocalTime.of(h, m, s, n))
			case '{LocalTime.of(${Expr(h)}, ${Expr(m)}, ${Expr(s)})} => Some(LocalTime.of(h, m, s))
			case '{LocalTime.of(${Expr(h)}, ${Expr(m)})} => Some(LocalTime.of(h, m))
			case _ => None
		}
	}

}