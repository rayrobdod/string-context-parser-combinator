package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import name.rayrobdod.stringContextParserCombinator._
import name.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import name.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

trait TimeParsers[Expr[_], Type[_]] {
	def localDate: Parser[Expr, Type, Expr[LocalDate]]
	def localTime: Parser[Expr, Type, Expr[LocalTime]]
	def localDateTime: Parser[Expr, Type, Expr[LocalDateTime]]
}

object TimeParsers {

	val digit:Interpolator[Any, Digit] = {
		Interpolator.charIn('0' to '9').map(Digit.apply _).opaque("AsciiDigit")
	}
	val sign:Interpolator[Any, Sign] = {
		Interpolator.charIn("-+").optionally().map({x => new Sign(x != Some('-'))})
	}

	def intTwoDigits(min:Int, max:Int):Interpolator[Any, Int] = (digit.repeat(2, 2))
		.map(_.value)
		.filter(x => min <= x && x <= max, f"${min}%02d <= $$value <= ${max}%02d")
		.opaque(f"${min}%02d <= $$value <= ${max}%02d")


	def apply[Expr[+_], ToExpr[_], Type[_]](
		leaves:Parser.Parsers[Expr, ToExpr, Type]
	)(
		yearMonthFlatMap: Expr[YearMonth] => leaves.Interpolator[Expr[LocalDate]],
		partsToLocalTime: (Expr[Int], Expr[Int], Expr[Int], Expr[Int]) => Expr[LocalTime],
		ofDayOfMonth: Expr[Int] => Expr[DayOfMonth],
	)(implicit
		toExprMapping: typeclass.ToExprMapping[Expr, ToExpr, Type],
		type_Int: Type[Int],
		type_Year: Type[Year],
		type_Month: Type[Month],
		type_DayOfMonth: Type[DayOfMonth],
		type_YearMonth: Type[YearMonth],
		type_LocalDate: Type[LocalDate],
		type_LocalTime: Type[LocalTime],
		toExpr_Int: ToExpr[Int],
		toExpr_Year: ToExpr[Year],
		toExpr_Month: ToExpr[Month],
		eithered_SymmetricExprInt: typeclass.BiEithered[Expr, Expr[Int], Expr[Int], Expr[Int]],
		eithered_SymmetricExprYear: typeclass.BiEithered[Expr, Expr[Year], Expr[Year], Expr[Year]],
		eithered_SymmetricExprMonth: typeclass.BiEithered[Expr, Expr[Month], Expr[Month], Expr[Month]],
		eithered_SymmetricExprYearMonth: typeclass.BiEithered[Expr, Expr[YearMonth], Expr[YearMonth], Expr[YearMonth]],
		eithered_SymmetricExprLocalDate: typeclass.BiEithered[Expr, Expr[LocalDate], Expr[LocalDate], Expr[LocalDate]],
		eithered_SymmetricExprLocalTime: typeclass.BiEithered[Expr, Expr[LocalTime], Expr[LocalTime], Expr[LocalTime]],
		sequenced_YearMonth: typeclass.BiSequenced[Expr[Year], Expr[Month], Expr[YearMonth]],
		sequenced_LocalDate: typeclass.ContraSequenced[Expr[YearMonth], Expr[Int], Expr[LocalDate]],
		sequenced_LocalDateTime: typeclass.BiSequenced[Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]]
	):TimeParsers[Expr, Type] = new TimeParsers[Expr, Type] {
		import leaves._

		val exprConstZero = toExprMapping(0, toExpr_Int, type_Int)

		implicit def str2parser(str:String):Parser[Unit] = isString(str)
		implicit def str2interpolator(str:String):Interpolator[Unit] = isString(str).toInterpolator
		implicit def str2extractor(str:String):Extractor[Unit] = isString(str).toExtractor

		val year:Parser[Expr[Year]] = {
			val literal:Parser[Expr[Year]] = {
				(sign <~> digit.repeat(1, 9))
					.opaque("\"-999999999\"-\"999999999\"")
					.map(Year.of _)
					.mapToExpr[Year, Expr, ToExpr, Type]
					.extractorAtom[Expr, Type, Year]
			}
			val variable:Parser[Expr[Year]] = ofType[Year]
			variable <|> literal
		}

		val month:Parser[Expr[Month]] = {
			val literal:Parser[Expr[Month]] = {
				intTwoDigits(1, 12)
					.map(Month.of _)
					.mapToExpr[Month, Expr, ToExpr, Type]
					.extractorAtom[Expr, Type, Month]
			}
			val variable:Parser[Expr[Month]] = ofType[Month]
			variable <|> literal
		}

		val yearMonth:Parser[Expr[YearMonth]] = {
			val literal:Parser[Expr[YearMonth]] = (year <~> "-" <~> month)
			val variable:Parser[Expr[YearMonth]] = ofType[YearMonth]
			variable <|> literal
		}

		val localDate:Parser[Expr[LocalDate]] = {
			val YearMonthVariantP:Parser[Expr[LocalDate]] = {
				val interpolator:Interpolator[Expr[LocalDate]] = (yearMonth <~ "-")
					.flatMap(yearMonthFlatMap)
				val extractor:Extractor[Expr[LocalDate]] = yearMonth.toExtractor
					.<~("-")
					.<~>(
						intTwoDigits(1, 31)
							.mapToExpr[Int, Expr, ToExpr, Type]
							.extractorAtom[Expr, Type, Int]
							.toExtractor
							.orElse(ofType[DayOfMonth].toExtractor.contramap(ofDayOfMonth))
						)

				paired(interpolator, extractor)
			}
			val variable:Parser[Expr[LocalDate]] = ofType[LocalDate]
			variable <|> YearMonthVariantP
		}

		val hour:Interpolator[Expr[Int]] = {
			val literal:Interpolator[Expr[Int]] = {
				intTwoDigits(0, 23)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			literal
		}

		val minute:Interpolator[Expr[Int]] = {
			val literal:Interpolator[Expr[Int]] = {
				intTwoDigits(0, 59)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			literal
		}

		val second:Interpolator[Expr[Int]] = minute

		val nano:Interpolator[Expr[Int]] = {
			val literal = charIn('0' to '9').toInterpolator.repeat(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.mapToExpr[Int, Expr, ToExpr, Type]
			literal
		}

		val localTime:Parser[Expr[LocalTime]] = {
			val literal:Parser[Expr[LocalTime]] = (
				hour <~> ":" <~> minute <~> (str2interpolator(":") <~> second <~> (
						str2interpolator(".") <~> nano).optionally()).optionally()
				)
				.map({hmsn =>
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((exprConstZero, None))
					val nano = n.getOrElse(exprConstZero)

					partsToLocalTime(hour, minute, second, nano)
				})
				.extractorAtom[Expr, Type, LocalTime]
			val variable:Parser[Expr[LocalTime]] = ofType[LocalTime]
			variable <|> literal
		}

		val localDateTime:Parser[Expr[LocalDateTime]] = {
			(localDate <~> "T" <~> localTime)
		}
	}
}
