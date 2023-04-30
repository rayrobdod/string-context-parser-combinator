package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

trait TimeParsers[Expr[_], Type[_]] {
	def localDate: Parser[Expr, Type, Expr[LocalDate]]
	def localTime: Parser[Expr, Type, Expr[LocalTime]]
	def localDateTime: Parser[Expr, Type, Expr[LocalDateTime]]
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
		leaves:Parser.Parsers[Expr, ToExpr, Type]
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
		eithered_SymmetricExprYear: typeclass.BiEithered[Expr, Expr[Year], Expr[Year], Expr[Year]],
		eithered_SymmetricExprMonth: typeclass.BiEithered[Expr, Expr[Month], Expr[Month], Expr[Month]],
		eithered_SymmetricExprYearMonth: typeclass.BiEithered[Expr, Expr[YearMonth], Expr[YearMonth], Expr[YearMonth]],
		eithered_SymmetricExprLocalDate: typeclass.BiEithered[Expr, Expr[LocalDate], Expr[LocalDate], Expr[LocalDate]],
		eithered_SymmetricExprLocalTime: typeclass.BiEithered[Expr, Expr[LocalTime], Expr[LocalTime], Expr[LocalTime]],
		sequenced_YearMonth: typeclass.BiSequenced[Expr[Year], Expr[Month], Expr[YearMonth]],
		sequenced_LocalDate: typeclass.ContraSequenced[Expr[YearMonth], Expr[Int], Expr[LocalDate]],
		sequenced_LocalDateTime: typeclass.BiSequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]]
	):TimeParsers[Expr, Type] = {
		import leaves._

		import scala.language.implicitConversions
		implicit def str2parser(str:String):Parser[Unit] = IsString(str)
		implicit def str2interpolator(str:String):Interpolator[Unit] = IsString(str).toInterpolator
		implicit def str2extractor(str:String):Extractor[Unit] = IsString(str).toExtractor

		val YearP:Parser[Expr[Year]] = {
			val LiteralP:Parser[Expr[Year]] = {
				(sign andThen digit.repeat(1, 9))
					.opaque("\"-999999999\"-\"999999999\"")
					.map(Year.of _)
					.mapToExpr[Year, Expr, ToExpr, Type]
					.extractorAtom[Expr, Type, Year]
			}
			val VariableP:Parser[Expr[Year]] = OfType[Year]
			VariableP orElse LiteralP
		}

		val MonthP:Parser[Expr[Month]] = {
			val LiteralP:Parser[Expr[Month]] = {
				intTwoDigits(1, 12)
					.map(Month.of _)
					.mapToExpr[Month, Expr, ToExpr, Type]
					.extractorAtom[Expr, Type, Month]
			}
			val VariableP:Parser[Expr[Month]] = OfType[Month]
			VariableP orElse LiteralP
		}

		val YearMonthP:Parser[Expr[YearMonth]] = {
			val PartsP:Parser[Expr[YearMonth]] = (YearP andThen "-" andThen MonthP)
			val VariableP:Parser[Expr[YearMonth]] = OfType[YearMonth]
			VariableP orElse PartsP
		}

		val LocalDateP:Parser[Expr[LocalDate]] = {
			val YearMonthVariantP:Parser[Expr[LocalDate]] = {
				val interpolator:Interpolator[Expr[LocalDate]] = (YearMonthP andThen "-")
					.flatMap(yearMonthFlatMap)
				val extractor:Extractor[Expr[LocalDate]] = YearMonthP.toExtractor
					.andThen("-")(typeclass.ContraSequenced.genericUnit[Expr[YearMonth]])
					.andThen(
						intTwoDigits(1, 31)
							.mapToExpr[Int, Expr, ToExpr, Type]
							.extractorAtom[Expr, Type, Int]
							.toExtractor
						)

				Paired(interpolator, extractor)
			}
			val VariableP:Parser[Expr[LocalDate]] = OfType[LocalDate]
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
			val LiteralP = CharIn('0' to '9').toInterpolator.repeat(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.mapToExpr[Int, Expr, ToExpr, Type]
			LiteralP
		}

		val LocalTimeP:Parser[Expr[LocalTime]] = {
			val LiteralP:Parser[Expr[LocalTime]] = (
				HourP andThen str2interpolator(":") andThen MinuteP andThen (
							str2interpolator(":") andThen SecondP andThen (str2interpolator(".") andThen NanoP).optionally()).optionally()
				)
				.map({hmsn =>
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((exprConstZero, None))
					val nano = n.getOrElse(exprConstZero)

					partsToLocalTime(hour, minute, second, nano)
				})
				.extractorAtom[Expr, Type, LocalTime]
			val VariableP:Parser[Expr[LocalTime]] = OfType[LocalTime]
			VariableP orElse LiteralP
		}

		val LocalDateTimeP:Parser[Expr[LocalDateTime]] = {
			(LocalDateP andThen "T" andThen LocalTimeP)
		}

		new TimeParsers[Expr, Type] {
			override def localDate: Parser[Expr[LocalDate]] = LocalDateP
			override def localTime: Parser[Expr[LocalTime]] = LocalTimeP
			override def localDateTime: Parser[Expr[LocalDateTime]] = LocalDateTimeP
		}
	}
}
