package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.Predef.charWrapper
import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator.{Parsers => scpcParsers, _}
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Digit.given_Repeated
import com.rayrobdod.stringContextParserCombinatorExample.datetime.Sign.given_Sequenced_Sign_Digit

/** Implicit methods to convert things to parsers or to add symbolic methods to parsers */
private[datetime] trait ParsersImplictly extends scpcParsers {
	import scala.language.implicitConversions
	implicit def str2parser(str:String):Parser[Unit] = this.IsString(str)
	implicit def type2parser[A](tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] = this.OfType(tpe)
	implicit def parserWithSymbolic[A](psr:Parser[A]) = new ParserWithSymbolic[ctx.Expr[_], A](psr)
	implicit def str2parserWithSymbolic(str:String) = this.parserWithSymbolic(this.str2parser(str))
	implicit def type2parserWithSymbolic[A](tpe:ctx.TypeTag[A]) = this.parserWithSymbolic(this.OfType(tpe))
}

/** Adds symbolic methods to Parsers */
private[datetime] class ParserWithSymbolic[U, A](val backing:Parser[U, A]) extends AnyVal {
	def ~[B, Z](rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThen(rhs)(ev)
	def ~/[B, Z](rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThenWithCut(rhs)(ev)
	def |[Z >: A](rhs:Parser[U, Z]) = backing.orElse(rhs)
	def rep[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:typelevel.Repeated[A, Z]) = backing.repeat(min, max)(ev)
	def opt[Z](implicit ev:typelevel.Optionally[A, Z]) = backing.optionally(ev)
}

object MacroImpl {
	private[this] object Name {
		def unapply(input:scala.reflect.api.Universe#Name):Option[String] = Option(input.decodedName.toString)
	}

	private[this] trait Parsers extends scpcParsers with ParsersImplictly with TimeLiftables with TimeUnliftables {
		implicit object sequenced_YearMonth extends typelevel.Sequenced[ctx.Expr[Year], ctx.Expr[Month], ctx.Expr[YearMonth]] {
			def aggregate(left:ctx.Expr[Year], right:ctx.Expr[Month]):ctx.Expr[YearMonth] = {
				import ctx.universe._
				(left, right) match {
					case (Expr(`unliftYear`(year)), Expr(`unliftMonth`(month))) => ctx.Expr[YearMonth](liftYearMonth(year.atMonth(month)))
					case (year, month) => ctx.Expr[YearMonth](q"$year.atMonth($month)")
				}
			}
		}

		implicit object sequenced_LocalDateTime extends typelevel.Sequenced[ctx.Expr[LocalDate], ctx.Expr[LocalTime], ctx.Expr[LocalDateTime]]{
			def aggregate(left:ctx.Expr[LocalDate], right:ctx.Expr[LocalTime]):ctx.Expr[LocalDateTime] = {
				import ctx.universe._
				(left, right) match {
					case (Expr(`unliftLocalDate`(date)), Expr(`unliftLocalTime`(time))) => ctx.Expr[LocalDateTime](liftLocalDateTime(date.atTime(time)))
					case (date, Expr(`unliftLocalTimeParts`(h, m, s, n))) => ctx.Expr[LocalDateTime](q"$date.atTime($h, $m, $s, $n)")
					case (date, time) => ctx.Expr[LocalDateTime](q"$date.atTime($time)")
				}
			}
		}

		val digit:Parser[Digit] = CharIn('0' to '9').map(Digit.apply _).opaque("AsciiDigit")
		val sign:Parser[Sign] = CharIn("-+").opt.map({x => new Sign(x != Some('-'))})

		def Int2Digits(min:Int, max:Int) = (digit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, String.format("""%02d <= $value <= %02d""", Integer.valueOf(min), Integer.valueOf(max)))

		def YearP:Parser[ctx.Expr[Year]] = {
			val LiteralP:Parser[ctx.Expr[Year]] = {
				(sign ~ digit.rep(1, 9))
					.opaque("\"-999999999\"-\"999999999\"")
					.map({x => ctx.Expr[Year](liftYear(Year.of(x)))})
			}
			val VariableP:Parser[ctx.Expr[Year]] = OfType(ctx.typeTag[Year])
			VariableP | LiteralP
		}

		def MonthP:Parser[ctx.Expr[Month]] = {
			val LiteralP:Parser[ctx.Expr[Month]] = {
				Int2Digits(1, 12)
					.map(Month.of _)
					.map({x => ctx.Expr[Month](liftMonth(x))})
			}
			val VariableP:Parser[ctx.Expr[Month]] = OfType(ctx.typeTag[Month])
			VariableP | LiteralP
		}

		def YearMonthP:Parser[ctx.Expr[YearMonth]] = {
			val PartsP:Parser[ctx.Expr[YearMonth]] = (YearP ~ "-" ~/ MonthP)
			val VariableP:Parser[ctx.Expr[YearMonth]] = OfType(ctx.typeTag[YearMonth])
			VariableP | PartsP
		}

		def LocalDateP:Parser[ctx.Expr[LocalDate]] = {
			val YearMonthVariantP:Parser[ctx.Expr[LocalDate]] = (YearMonthP ~ "-").flatMap({ymExpr =>
				import ctx.universe._
				ymExpr match {
					case Expr(`unliftYearMonth`(ym)) => {
						Int2Digits(1, ym.lengthOfMonth).map(day => ctx.Expr[LocalDate](liftLocalDate(ym.atDay(day))))
					}
					case ctx.Expr(Apply(Select(_, Name("atMonth")), List(`unliftMonth`(month)))) => {
						Int2Digits(1, month.maxLength).map({day =>
							ctx.Expr[LocalDate](q"$ymExpr.atDay($day)")
						})
					}
					case ymExpr => {
						Int2Digits(1, 31).map({day =>
							ctx.Expr[LocalDate](q"$ymExpr.atDay($day)")
						})
					}
				}
			})
			val VariableP:Parser[ctx.Expr[LocalDate]] = OfType(ctx.typeTag[LocalDate])
			VariableP | YearMonthVariantP
		}

		def HourP:Parser[ctx.Expr[Int]] = {
			val LiteralP:Parser[ctx.Expr[Int]] = {
				Int2Digits(0, 23)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def MinuteP:Parser[ctx.Expr[Int]] = {
			val LiteralP:Parser[ctx.Expr[Int]] = {
				Int2Digits(0, 59)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def SecondP:Parser[ctx.Expr[Int]] = MinuteP

		def NanoP:Parser[ctx.Expr[Int]] = {
			val LiteralP = CharIn('0' to '9').rep(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			LiteralP
		}

		def LocalTimeP:Parser[ctx.Expr[LocalTime]] = {
			import ctx.universe.Quasiquote
			val LiteralP:Parser[ctx.Expr[LocalTime]] = (HourP ~ ":" ~/ MinuteP ~ (":" ~/ SecondP ~ ("." ~/ NanoP).opt).opt)
				.map({hmsn =>
					val constZero = ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(0)))
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((constZero, None))
					val nano = n.getOrElse(constZero)

					ctx.Expr[LocalTime](q"java.time.LocalTime.of($hour, $minute, $second, $nano)")
				})
			val VariableP:Parser[ctx.Expr[LocalTime]] = OfType(ctx.typeTag[LocalTime])
			VariableP | LiteralP
		}

		def LocalDateTimeP:Parser[ctx.Expr[LocalDateTime]] = {
			(LocalDateP ~ "T" ~/ LocalTimeP)
		}
	}

	private[this] val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

	def stringContext_localdate(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDate] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateP ~ this.End)
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localtime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalTimeP ~ this.End)
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localdatetime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateTimeP ~ this.End)
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}
}
