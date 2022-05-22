package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.language.implicitConversions
import scala.Predef.charWrapper
import scala.quoted._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinatorExample.datetime.{Digit, Digits}
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given
import com.rayrobdod.stringContextParserCombinatorExample.datetime.ExprConversions.given
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given

object MacroImpl {
	private given Conversion[String, Parsers.Parser[Unit]] = Parsers.IsString(_)

	private given given_Sequenced_Expr_YearMonth(using Quotes):typelevel.Sequenced[Expr[Year], Expr[Month], Expr[YearMonth]] with {
		def aggregate(left:Expr[Year], right:Expr[Month]):Expr[YearMonth] = (left, right) match {
			case (Expr(year), Expr(month)) => '{YearMonth.of(${Expr(year.getValue())}, ${Expr(month.getValue())})}
			case (Expr(year), month) => '{YearMonth.of(${Expr(year.getValue())}, $month)}
			case (year, Expr(month)) => '{$year.atMonth(${Expr(month.getValue())})}
			case (year, month) => '{$year.atMonth($month)}
		}
	}

	private given given_Sequenced_Expr_DateTime(using Quotes):typelevel.Sequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]] with {
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

	/** Adds symbolic methods to Parsers */
	extension [U, A, B, Z] (backing:Parser[U, A])
		def ~(rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThen(rhs)(ev)
		def ~/(rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThenWithCut(rhs)(ev)
		def |(rhs:Parser[U, B])(implicit ev:typelevel.Eithered[A,B,Z]) = backing.orElse(rhs)(ev)

	/** Adds symbolic methods to Parsers */
	extension [U, A, Z] (backing:Parser[U, A])
		def rep(min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:typelevel.Repeated[A, Z]) = backing.repeat(min, max)(ev)
		def opt(implicit ev:typelevel.Optionally[A, Z]) = backing.optionally()(ev)



	import com.rayrobdod.stringContextParserCombinator.Parsers._

	private val digit:Parser[Digit] = CharIn('0' to '9')
		.map(Digit.apply _)
		.opaque("AsciiDigit")

	private val sign:Parser[Sign] = CharIn("+-").opt.map({x => Sign(x != Some('-'))})

	private[this] def Int2Digits(min:Int, max:Int) = (digit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, f"${min}%02d <= $$value <= ${max}%02d")
			.opaque(f"${min}%02d <= $$value <= ${max}%02d")

	private[this] def YearP(using Quotes):Parser[Expr[Year]] = {
		val LiteralP:Parser[Expr[Year]] = {
			(sign ~ digit.rep(1, 9))
				.opaque("\"-999999999\"-\"999999999\"")
				.map(Year.of _)
				.map(Expr.apply _)
		}
		val VariableP:Parser[Expr[Year]] = OfType[Year]
		VariableP | LiteralP
	}

	private[this] def MonthP(using Quotes):Parser[Expr[Month]] = {
		val LiteralP:Parser[Expr[Month]] = {
			Int2Digits(1, 12)
				.map(Month.of _)
				.map(Expr.apply _)
		}
		val VariableP:Parser[Expr[Month]] = OfType[Month]
		VariableP | LiteralP
	}

	private[this] def YearMonthP(using Quotes):Parser[Expr[YearMonth]] = {
		val PartsP:Parser[Expr[YearMonth]] = (YearP ~ "-" ~/ MonthP)
		val VariableP:Parser[Expr[YearMonth]] = OfType[YearMonth]
		VariableP | PartsP
	}

	private[this] def LocalDateP(using Quotes):Parser[Expr[LocalDate]] = {
		val YearMonthVariantP = (YearMonthP ~ "-").flatMap(_ match {
			case Expr(ym) => Int2Digits(1, ym.lengthOfMonth).map(day => Expr(ym.atDay(day)))
			case '{YearMonth.of($y:Int, ${Expr(m)}:Int)} => Int2Digits(1, Month.of(m).maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, ${Expr(day)})})
			case '{YearMonth.of($y:Int, ${Expr(m)}:Month)} => Int2Digits(1, m.maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, ${Expr(day)})})
			case '{($year:Year).atMonth(${Expr(m)}:Int)} => Int2Digits(1, Month.of(m).maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay(${Expr(day)})})
			case '{($year:Year).atMonth(${Expr(m)}:Month)} => Int2Digits(1, m.maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay(${Expr(day)})})
			case '{YearMonth.of($y:Int, $m:Int)} => Int2Digits(1, 31).map(day => '{LocalDate.of($y, $m, ${Expr(day)})})
			case '{YearMonth.of($y:Int, $m:Month)} => Int2Digits(1, 31).map(day => '{LocalDate.of($y, $m, ${Expr(day)})})
			case ym => Int2Digits(1, 31).map(day => '{$ym.atDay(${Expr(day)})})
		})
		val VariableP:Parser[Expr[LocalDate]] = OfType[LocalDate]
		VariableP | YearMonthVariantP
	}

	private[this] def HourP(using Quotes):Parser[Expr[Int]] = {
		val LiteralP:Parser[Expr[Int]] = {
			Int2Digits(0, 23)
				.map(x => Expr(x))
		}
		LiteralP
	}

	private[this] def MinuteP(using Quotes):Parser[Expr[Int]] = {
		val LiteralP:Parser[Expr[Int]] = {
			Int2Digits(0, 59)
				.map(x => Expr(x))
		}
		LiteralP
	}

	private[this] def SecondP(using Quotes):Parser[Expr[Int]] = MinuteP

	private[this] def NanoP(using Quotes):Parser[Expr[Int]] = {
		val LiteralP = CharIn('0' to '9').rep(1, 9)
			.map(x => s"${x}000000000".substring(0, 9))
			.map(Integer.parseInt _)
			.opaque("\"0\"-\"999999999\"")
			.map(x => Expr(x))
		LiteralP
	}

	private[this] def LocalTimeP(using Quotes):Parser[Expr[LocalTime]] = {
		val LiteralP:Parser[Expr[LocalTime]] = (HourP ~ ":" ~/ MinuteP ~ (":" ~/ SecondP ~ ("." ~/ NanoP).opt).opt)
			.map({hmsn =>
				val constZero = Expr(0)
				val (hm, sn) = hmsn
				val (hour, minute) = hm
				val (second, n) = sn.getOrElse((constZero, None))
				val nano = n.getOrElse(constZero)

				'{ LocalTime.of($hour, $minute, $second, $nano) }
			})
		val VariableP:Parser[Expr[LocalTime]] = OfType[LocalTime]
		VariableP | LiteralP
	}

	private[this] def LocalDateTimeP(using Quotes):Parser[Expr[LocalDateTime]] = {
		(LocalDateP ~ "T" ~/ LocalTimeP)
	}

	def stringContext_localdate(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDate] = {
		val Aggregate = (this.LocalDateP ~ Parsers.End)
		macroimpl[LocalDate](Aggregate)(sc, args)
	}

	def stringContext_localtime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalTime] = {
		val Aggregate = (this.LocalTimeP ~ Parsers.End)
		macroimpl[LocalTime](Aggregate)(sc, args)
	}

	def stringContext_localdatetime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDateTime] = {
		val Aggregate = (this.LocalDateTimeP ~ Parsers.End)
		macroimpl[LocalDateTime](Aggregate)(sc, args)
	}
}
