package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.quoted._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated

object MacroImpl {
	import ExprConversions.given

	private given given_Sequenced_Expr_YearMonth(using Quotes):typeclass.Sequenced[Expr[Year], Expr[Month], Expr[YearMonth]] with {
		def aggregate(left:Expr[Year], right:Expr[Month]):Expr[YearMonth] = (left, right) match {
			case (Expr(year), Expr(month)) => '{YearMonth.of(${Expr(year.getValue())}, ${Expr(month.getValue())})}
			case (Expr(year), month) => '{YearMonth.of(${Expr(year.getValue())}, $month)}
			case (year, Expr(month)) => '{$year.atMonth(${Expr(month.getValue())})}
			case (year, month) => '{$year.atMonth($month)}
		}
	}

	private given given_Sequenced_Expr_DateTime(using Quotes):typeclass.Sequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]] with {
		def aggregate(left:Expr[LocalDate], right:Expr[LocalTime]):Expr[LocalDateTime] = (left, right) match {
			case (Expr(date), Expr(time)) => '{
				LocalDateTime.of(
					${Expr(date.getYear())}, ${Expr(date.getMonthValue())}, ${Expr(date.getDayOfMonth())},
					${Expr(time.getHour())}, ${Expr(time.getMinute())}, ${Expr(time.getSecond())}, ${Expr(time.getNano())},
				)
			}
			case ('{LocalDate.of($y:Int, $mo:Int, $d:Int)}, '{LocalTime.of($h, $mi, $s, $n)}) => '{
				LocalDateTime.of($y, $mo, $d, $h, $mi, $s, $n)
			}
			case ('{LocalDate.of($y:Int, $mo:Month, $d:Int)}, '{LocalTime.of($h, $mi, $s, $n)}) => '{
				LocalDateTime.of($y, $mo, $d, $h, $mi, $s, $n)
			}
			case (date, Expr(time)) => '{
				$date.atTime(
					${Expr(time.getHour())}, ${Expr(time.getMinute())}, ${Expr(time.getSecond())}, ${Expr(time.getNano())}
				)
			}
			case (date, time) => '{ $date.atTime($time) }
		}
	}

	import Interpolator.End
	import TimeParsers.intTwoDigits


	private def timeParsers(using Quotes) = {
		val leafParsers:Interpolator.Interpolators[quoted.Expr, quoted.ToExpr, quoted.Type] =
					Interpolator.macroInterpolators

		TimeParsers(leafParsers)(
			Expr(0),
			_ match {
				case Expr(ym) => intTwoDigits(1, ym.lengthOfMonth).map(day => Expr(ym.atDay(day)))
				case '{YearMonth.of($y:Int, ${Expr(m)}:Int)} => intTwoDigits(1, Month.of(m).maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, ${Expr(day)})})
				case '{YearMonth.of($y:Int, ${Expr(m)}:Month)} => intTwoDigits(1, m.maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, ${Expr(day)})})
				case '{($year:Year).atMonth(${Expr(m)}:Int)} => intTwoDigits(1, Month.of(m).maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay(${Expr(day)})})
				case '{($year:Year).atMonth(${Expr(m)}:Month)} => intTwoDigits(1, m.maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay(${Expr(day)})})
				case '{YearMonth.of($y:Int, $m:Int)} => intTwoDigits(1, 31).map(day => '{LocalDate.of($y, $m, ${Expr(day)})})
				case '{YearMonth.of($y:Int, $m:Month)} => intTwoDigits(1, 31).map(day => '{LocalDate.of($y, $m, ${Expr(day)})})
				case ym => intTwoDigits(1, 31).map(day => '{$ym.atDay(${Expr(day)})})
			},
			(hour, minute, second, nano) =>
				'{ LocalTime.of($hour, $minute, $second, $nano) }
		)(using
			typeclass.ToExprMapping.toExprQuoted,
			implicitly[quoted.Type[Int]],
			implicitly[quoted.Type[Year]],
			implicitly[quoted.Type[Month]],
			implicitly[quoted.Type[YearMonth]],
			implicitly[quoted.Type[LocalDate]],
			implicitly[quoted.Type[LocalTime]],
			implicitly[quoted.ToExpr[Int]],
			implicitly[quoted.ToExpr[Year]],
			implicitly[quoted.ToExpr[Month]],
			implicitly[typeclass.Sequenced[Expr[Year], Expr[Month], Expr[YearMonth]]],
			implicitly[typeclass.Sequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]]],
		)
	}


	def interpolate_localDate(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDate] = {
		(timeParsers.localDate andThen End).interpolate(sc, args)
	}

	def interpolate_localTime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalTime] = {
		(timeParsers.localTime andThen End).interpolate(sc, args)
	}

	def interpolate_localDateTime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDateTime] = {
		(timeParsers.localDateTime andThen End).interpolate(sc, args)
	}
}

extension (inline sc:scala.StringContext)
	inline def localdate(inline args:Any*):LocalDate =
		${MacroImpl.interpolate_localDate('sc, 'args)}
	inline def localtime(inline args:Any*):LocalTime =
		${MacroImpl.interpolate_localTime('sc, 'args)}
	inline def localdatetime(inline args:Any*):LocalDateTime =
		${MacroImpl.interpolate_localDateTime('sc, 'args)}

extension (sc:scala.StringContext)
	def localdate2(args:Any*):LocalDate =
		IdImpl.interpolate_localDate(sc, args*)
	def localtime2(args:Any*):LocalTime =
		IdImpl.interpolate_localTime(sc, args*)
	def localdatetime2(args:Any*):LocalDateTime =
		IdImpl.interpolate_localDateTime(sc, args*)
