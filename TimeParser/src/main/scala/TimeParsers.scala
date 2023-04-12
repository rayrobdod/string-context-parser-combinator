package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

trait TimeParsers[Expr[_]] {
	def localDate: Interpolator[Expr[Any], Expr[LocalDate]]
	def localTime: Interpolator[Expr[Any], Expr[LocalTime]]
	def localDateTime: Interpolator[Expr[Any], Expr[LocalDateTime]]
}

object TimeParsers {

	val digit:Interpolator[Any, Digit] = {
		Interpolator.CharIn('0' to '9').map(Digit.apply _).opaque("AsciiDigit")
	}
	val sign:Interpolator[Any, Sign] = {
		Interpolator.CharIn("-+").optionally().map({x => new Sign(x != Some('-'))})
	}

	def intTwoDigits(min:Int, max:Int):Interpolator[Any, Int] = (digit.repeat(2, 2))
		.map(_.value)
		.filter(x => min <= x && x <= max, f"${min}%02d <= $$value <= ${max}%02d")
		.opaque(f"${min}%02d <= $$value <= ${max}%02d")


	def apply[Expr[+_], ToExpr[_], Type[_]](
		leaves:Interpolator.Interpolators[Expr, ToExpr, Type]
	)(
		exprConstZero: Expr[Int],
		yearMonthFlatMap: Expr[YearMonth] => leaves.Interpolator[Expr[LocalDate]],
		partsToLocalTime: (Expr[Int], Expr[Int], Expr[Int], Expr[Int]) => Expr[LocalTime]
	)(implicit
		toExprMapping: typeclass.ToExprMapping[Expr, ToExpr, Type],
		type_Int: Type[Int],
		type_Year: Type[Year],
		type_Month: Type[Month],
		type_YearMonth: Type[YearMonth],
		type_LocalDate: Type[LocalDate],
		type_LocalTime: Type[LocalTime],
		toExpr_Int: ToExpr[Int],
		toExpr_Year: ToExpr[Year],
		toExpr_Month: ToExpr[Month],
		sequenced_YearMonth: typeclass.Sequenced[Expr[Year], Expr[Month], Expr[YearMonth]],
		sequenced_LocalDateTime: typeclass.Sequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]]
	):TimeParsers[Expr] = {
		import leaves._

		import scala.language.implicitConversions
		implicit def str2parser(str:String):Interpolator[Unit] = IsString(str)

		val YearP:Interpolator[Expr[Year]] = {
			val LiteralP:Interpolator[Expr[Year]] = {
				(sign andThen digit.repeat(1, 9))
					.opaque("\"-999999999\"-\"999999999\"")
					.map(Year.of _)
					.mapToExpr[Year, Expr, ToExpr, Type]
			}
			val VariableP:Interpolator[Expr[Year]] = OfType[Year]
			VariableP orElse LiteralP
		}

		val MonthP:Interpolator[Expr[Month]] = {
			val LiteralP:Interpolator[Expr[Month]] = {
				intTwoDigits(1, 12)
					.map(Month.of _)
					.mapToExpr[Month, Expr, ToExpr, Type]
			}
			val VariableP:Interpolator[Expr[Month]] = OfType[Month]
			VariableP orElse LiteralP
		}

		val YearMonthP:Interpolator[Expr[YearMonth]] = {
			val PartsP:Interpolator[Expr[YearMonth]] = (YearP andThen "-" andThen MonthP)
			val VariableP:Interpolator[Expr[YearMonth]] = OfType[YearMonth]
			VariableP orElse PartsP
		}

		val LocalDateP:Interpolator[Expr[LocalDate]] = {
			val YearMonthVariantP:Interpolator[Expr[LocalDate]] = (YearMonthP andThen "-")
				.flatMap(yearMonthFlatMap)
			val VariableP:Interpolator[Expr[LocalDate]] = OfType[LocalDate]
			VariableP orElse YearMonthVariantP
		}

		val HourP:Interpolator[Expr[Int]] = {
			val LiteralP:Interpolator[Expr[Int]] = {
				intTwoDigits(0, 23)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			LiteralP
		}

		val MinuteP:Interpolator[Expr[Int]] = {
			val LiteralP:Interpolator[Expr[Int]] = {
				intTwoDigits(0, 59)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			LiteralP
		}

		val SecondP:Interpolator[Expr[Int]] = MinuteP

		val NanoP:Interpolator[Expr[Int]] = {
			val LiteralP = CharIn('0' to '9').repeat(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.mapToExpr[Int, Expr, ToExpr, Type]
			LiteralP
		}

		val LocalTimeP:Interpolator[Expr[LocalTime]] = {
			val LiteralP:Interpolator[Expr[LocalTime]] = (
					HourP andThen ":" andThen MinuteP andThen (
							str2parser(":") andThen SecondP andThen (str2parser(".") andThen NanoP).optionally()).optionally()
				)
				.map({hmsn =>
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((exprConstZero, None))
					val nano = n.getOrElse(exprConstZero)

					partsToLocalTime(hour, minute, second, nano)
				})
			val VariableP:Interpolator[Expr[LocalTime]] = OfType[LocalTime]
			VariableP orElse LiteralP
		}

		val LocalDateTimeP:Interpolator[Expr[LocalDateTime]] = {
			(LocalDateP andThen "T" andThen LocalTimeP)
		}

		new TimeParsers[Expr] {
			override def localDate: Interpolator[Expr[LocalDate]] = LocalDateP
			override def localTime: Interpolator[Expr[LocalTime]] = LocalTimeP
			override def localDateTime: Interpolator[Expr[LocalDateTime]] = LocalDateTimeP
		}
	}
}
