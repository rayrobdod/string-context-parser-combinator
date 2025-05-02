package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.collection.immutable.Seq
import name.rayrobdod.stringContextParserCombinator._
import name.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import name.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

trait TimeParsers[Ctx, Expr[+_], Type[_]] {
	def localDate: Parser[Ctx, Expr, Type, Expr[LocalDate]]
	def localTime: Parser[Ctx, Expr, Type, Expr[LocalTime]]
	def localDateTime: Parser[Ctx, Expr, Type, Expr[LocalDateTime]]
}

object TimeParsers {

	def digit[Ctx, Expr[+_]](
		charIn:(Seq[Char] => Interpolator[Ctx, Expr[Any], Char])
	):Interpolator[Ctx, Expr[Any], Digit] = {
		charIn('0' to '9').map((x, _:Ctx) => Digit(x)).opaque("AsciiDigit")
	}
	def sign[Ctx, Expr[+_]](
		charIn:(Seq[Char] => Interpolator[Ctx, Expr[Any], Char])
	):Interpolator[Ctx, Expr[Any], Sign] = {
		charIn("-+").optionally().map({(x, _:Ctx) => new Sign(x != Some('-'))})
	}

	def intTwoDigits[Ctx, Expr[+_]](
		charIn:(Seq[Char] => Interpolator[Ctx, Expr[Any], Char])
	)(min:Int, max:Int):Interpolator[Ctx, Expr[Any], Int] = (digit(charIn).repeat(2, 2))
		.map((x, _:Ctx) => x.value)
		.filter((x, _:Ctx) => min <= x && x <= max, f"${min}%02d <= $$value <= ${max}%02d")
		.opaque(f"${min}%02d <= $$value <= ${max}%02d")


	def apply[Ctx, Expr[+_], ToExpr[_], Type[_]](
		leaves:Parser.Parsers[Ctx, Expr, ToExpr, Type]
	)(
		yearMonthFlatMap: (Expr[YearMonth], Ctx) => leaves.Interpolator[Expr[LocalDate]],
		partsToLocalTime: (Expr[Int], Expr[Int], Expr[Int], Expr[Int], Ctx) => Expr[LocalTime],
		ofDayOfMonth: (Expr[Int], Ctx) => Expr[DayOfMonth],
	)(implicit
		toExprMapping: typeclass.ToExprMapping[Ctx, Expr, ToExpr, Type],
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
		eithered_SymmetricExprInt: typeclass.BiEithered[Ctx, Expr, Expr[Int], Expr[Int], Expr[Int]],
		eithered_SymmetricExprYear: typeclass.BiEithered[Ctx, Expr, Expr[Year], Expr[Year], Expr[Year]],
		eithered_SymmetricExprMonth: typeclass.BiEithered[Ctx, Expr, Expr[Month], Expr[Month], Expr[Month]],
		eithered_SymmetricExprYearMonth: typeclass.BiEithered[Ctx, Expr, Expr[YearMonth], Expr[YearMonth], Expr[YearMonth]],
		eithered_SymmetricExprLocalDate: typeclass.BiEithered[Ctx, Expr, Expr[LocalDate], Expr[LocalDate], Expr[LocalDate]],
		eithered_SymmetricExprLocalTime: typeclass.BiEithered[Ctx, Expr, Expr[LocalTime], Expr[LocalTime], Expr[LocalTime]],
		sequenced_YearMonth: typeclass.BiSequenced[Ctx, Expr[Year], Expr[Month], Expr[YearMonth]],
		sequenced_LocalDate: typeclass.ContraSequenced[Ctx, Expr[YearMonth], Expr[Int], Expr[LocalDate]],
		sequenced_LocalDateTime: typeclass.BiSequenced[Ctx, Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]]
	):TimeParsers[Ctx, Expr, Type] = new TimeParsers[Ctx, Expr, Type] {
		import leaves._

		val exprConstZero = (ctx:Ctx) => toExprMapping(ctx, 0, toExpr_Int, type_Int)
		val charIn:(Seq[Char]) => Interpolator[Char] = (chars: Seq[Char]) => leaves.charIn(chars).toInterpolator

		implicit def str2parser(str:String):Parser[Unit] = isString(str)
		implicit def str2interpolator(str:String):Interpolator[Unit] = isString(str).toInterpolator
		implicit def str2extractor(str:String):Extractor[Unit] = isString(str).toExtractor

		val year:Parser[Expr[Year]] = {
			val literal:Parser[Expr[Year]] = {
				(sign(charIn) <~> digit(charIn).repeat(1, 9))
					.opaque("\"-999999999\"-\"999999999\"")
					.map((x, _:Ctx) => Year.of(x))
					.mapToExpr[Year, Expr, ToExpr, Type]
					.extractorAtom[Expr, Type, Year]
			}
			val variable:Parser[Expr[Year]] = ofType[Year]
			variable <|> literal
		}

		val month:Parser[Expr[Month]] = {
			val literal:Parser[Expr[Month]] = {
				intTwoDigits(charIn)(1, 12)
					.map((x, _:Ctx) => Month.of(x))
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
						intTwoDigits(charIn)(1, 31)
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
				intTwoDigits(charIn)(0, 23)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			literal
		}

		val minute:Interpolator[Expr[Int]] = {
			val literal:Interpolator[Expr[Int]] = {
				intTwoDigits(charIn)(0, 59)
					.mapToExpr[Int, Expr, ToExpr, Type]
			}
			literal
		}

		val second:Interpolator[Expr[Int]] = minute

		val nano:Interpolator[Expr[Int]] = {
			val literal = charIn('0' to '9').repeat(1, 9)
				.map((x, _:Ctx) => s"${x}000000000".substring(0, 9))
				.map((x, _:Ctx) => Integer.parseInt(x))
				.opaque("\"0\"-\"999999999\"")
				.mapToExpr[Int, Expr, ToExpr, Type]
			literal
		}

		val localTime:Parser[Expr[LocalTime]] = {
			val literal:Parser[Expr[LocalTime]] = (
				hour <~> ":" <~> minute <~> (str2interpolator(":") <~> second <~> (
						str2interpolator(".") <~> nano).optionally()).optionally()
				)
				.map({(hmsn, ctx:Ctx) =>
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((exprConstZero(ctx), None))
					val nano = n.getOrElse(exprConstZero(ctx))

					partsToLocalTime(hour, minute, second, nano, ctx)
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
