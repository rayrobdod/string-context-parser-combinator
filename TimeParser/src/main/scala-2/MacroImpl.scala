package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.Predef.charWrapper
import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator.{Parsers => scpcParsers, _}
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

/** Adds symbolic methods to Parsers */
private[datetime] class ParserWithOps[U, A](val backing:Parser[U, A]) extends AnyVal {
	def ~[B, Z](rhs:Parser[U, B])(implicit ev:typeclass.Sequenced[A,B,Z]) = backing.andThen(rhs)(ev)
	def |[Z >: A](rhs:Parser[U, Z]) = backing.orElse(rhs)
	def rep[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:typeclass.Repeated[A, Z]) = backing.repeat(min, max)(ev)
	def opt[Z](implicit ev:typeclass.Optionally[A, Z]) = backing.optionally()(ev)
}

final class MacroImpl(val c:Context {type PrefixType = DateTimeStringContext}) {
	private[this] object Name {
		def unapply(input:scala.reflect.api.Universe#Name):Option[String] = Option(input.decodedName.toString)
	}

	private[this] val leafParsers = scpcParsers(c)
	import leafParsers._
	private[this] val timeLiftables = TimeLiftables(c)
	import timeLiftables._
	private[this] val timeUnliftables = TimeUnliftables(c)
	import timeUnliftables._

	import scala.language.implicitConversions
	private[this] implicit def str2parser(str:String):Parser[Unit] = IsString(str)
	private[this] implicit def parserWithOps[A](psr:Parser[A]) = new ParserWithOps[c.Expr[_], A](psr)
	private[this] implicit def str2parserWithOps(str:String) = parserWithOps(str2parser(str))


	private[this] implicit object sequenced_YearMonth extends typeclass.Sequenced[c.Expr[Year], c.Expr[Month], c.Expr[YearMonth]] {
		def aggregate(left:c.Expr[Year], right:c.Expr[Month]):c.Expr[YearMonth] = {
			import c.universe._
			(left, right) match {
				case (Expr(`unliftYear`(year)), Expr(`unliftMonth`(month))) => c.Expr[YearMonth](liftYearMonth(year.atMonth(month)))
				case (year, month) => c.Expr[YearMonth](q"$year.atMonth($month)")
			}
		}
	}

	private[this] implicit object sequenced_LocalDateTime extends typeclass.Sequenced[c.Expr[LocalDate], c.Expr[LocalTime], c.Expr[LocalDateTime]]{
		def aggregate(left:c.Expr[LocalDate], right:c.Expr[LocalTime]):c.Expr[LocalDateTime] = {
			import c.universe._
			(left, right) match {
				case (Expr(`unliftLocalDate`(date)), Expr(`unliftLocalTime`(time))) => c.Expr[LocalDateTime](liftLocalDateTime(date.atTime(time)))
				case (date, Expr(`unliftLocalTimeParts`(h, m, s, n))) => c.Expr[LocalDateTime](q"$date.atTime($h, $m, $s, $n)")
				case (date, time) => c.Expr[LocalDateTime](q"$date.atTime($time)")
			}
		}
	}

	private[this] val digit:Parser[Digit] = CharIn('0' to '9').map(Digit.apply _).opaque("AsciiDigit")
	private[this] val sign:Parser[Sign] = CharIn("-+").opt.map({x => new Sign(x != Some('-'))})

	private[this] def Int2Digits(min:Int, max:Int) = (digit.rep(2, 2))
		.map(_.value)
		.filter(x => min <= x && x <= max, f"${min}%02d <= $$value <= ${max}%02d")
		.opaque(f"${min}%02d <= $$value <= ${max}%02d")

	private[this] def YearP:Parser[c.Expr[Year]] = {
		val LiteralP:Parser[c.Expr[Year]] = {
			(sign ~ digit.rep(1, 9))
				.opaque("\"-999999999\"-\"999999999\"")
				.map({x => c.Expr[Year](liftYear(Year.of(x)))})
		}
		val VariableP:Parser[c.Expr[Year]] = OfType[Year]
		VariableP | LiteralP
	}

	private[this] def MonthP:Parser[c.Expr[Month]] = {
		val LiteralP:Parser[c.Expr[Month]] = {
			Int2Digits(1, 12)
				.map(Month.of _)
				.map({x => c.Expr[Month](liftMonth(x))})
		}
		val VariableP:Parser[c.Expr[Month]] = OfType[Month]
		VariableP | LiteralP
	}

	private[this] def YearMonthP:Parser[c.Expr[YearMonth]] = {
		val PartsP:Parser[c.Expr[YearMonth]] = (YearP ~ "-" ~ MonthP)
		val VariableP:Parser[c.Expr[YearMonth]] = OfType[YearMonth]
		VariableP | PartsP
	}

	private[this] def LocalDateP:Parser[c.Expr[LocalDate]] = {
		val YearMonthVariantP:Parser[c.Expr[LocalDate]] = (YearMonthP ~ "-").flatMap({ymExpr =>
			import c.universe._
			ymExpr match {
				case Expr(`unliftYearMonth`(ym)) => {
					Int2Digits(1, ym.lengthOfMonth).map(day => c.Expr[LocalDate](liftLocalDate(ym.atDay(day))))
				}
				case c.Expr(Apply(Select(_, Name("atMonth")), List(`unliftMonth`(month)))) => {
					Int2Digits(1, month.maxLength).map({day =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
				case ymExpr => {
					Int2Digits(1, 31).map({day =>
						c.Expr[LocalDate](q"$ymExpr.atDay($day)")
					})
				}
			}
		})
		val VariableP:Parser[c.Expr[LocalDate]] = OfType[LocalDate]
		VariableP | YearMonthVariantP
	}

	private[this] def HourP:Parser[c.Expr[Int]] = {
		val LiteralP:Parser[c.Expr[Int]] = {
			Int2Digits(0, 23)
				.map(x => c.Expr(c.universe.Literal(c.universe.Constant(x))))
		}
		LiteralP
	}

	private[this] def MinuteP:Parser[c.Expr[Int]] = {
		val LiteralP:Parser[c.Expr[Int]] = {
			Int2Digits(0, 59)
				.map(x => c.Expr(c.universe.Literal(c.universe.Constant(x))))
		}
		LiteralP
	}

	private[this] def SecondP:Parser[c.Expr[Int]] = MinuteP

	private[this] def NanoP:Parser[c.Expr[Int]] = {
		val LiteralP = CharIn('0' to '9').rep(1, 9)
			.map(x => s"${x}000000000".substring(0, 9))
			.map(Integer.parseInt _)
			.opaque("\"0\"-\"999999999\"")
			.map(x => c.Expr(c.universe.Literal(c.universe.Constant(x))))
		LiteralP
	}

	private[this] def LocalTimeP:Parser[c.Expr[LocalTime]] = {
		import c.universe.Quasiquote
		val LiteralP:Parser[c.Expr[LocalTime]] = (HourP ~ ":" ~ MinuteP ~ (":" ~ SecondP ~ ("." ~ NanoP).opt).opt)
			.map({hmsn =>
				val constZero = c.Expr(c.universe.Literal(c.universe.Constant(0)))
				val (hm, sn) = hmsn
				val (hour, minute) = hm
				val (second, n) = sn.getOrElse((constZero, None))
				val nano = n.getOrElse(constZero)

				c.Expr[LocalTime](q"java.time.LocalTime.of($hour, $minute, $second, $nano)")
			})
		val VariableP:Parser[c.Expr[LocalTime]] = OfType[LocalTime]
		VariableP | LiteralP
	}

	private[this] def LocalDateTimeP:Parser[c.Expr[LocalDateTime]] = {
		(LocalDateP ~ "T" ~ LocalTimeP)
	}

	private[this] val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

	def stringContext_localdate(args:c.Expr[Any]*):c.Expr[LocalDate] = {
		(LocalDateP ~ End).interpolate(c)(extensionClassName)(args.toList)
	}

	def stringContext_localtime(args:c.Expr[Any]*):c.Expr[LocalTime] = {
		(LocalTimeP ~ End).interpolate(c)(extensionClassName)(args.toList)
	}

	def stringContext_localdatetime(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
		(LocalDateTimeP ~ End).interpolate(c)(extensionClassName)(args.toList)
	}
}
