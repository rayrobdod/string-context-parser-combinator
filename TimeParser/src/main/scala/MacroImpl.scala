package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.Predef.charWrapper
import com.rayrobdod.stringContextParserCombinator.{Parsers => scpcParsers, _}
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/** Implicit methods to convert things to parsers or to add symbolic methods to parsers */
trait ParsersImplictly extends scpcParsers {
	import scala.language.implicitConversions
	implicit def str2parser(str:String):Parser[Unit] = this.IsString(str)
	implicit def type2parser[A](tpe:ctx.TypeTag[A]):Parser[ctx.Expr[A]] = this.OfType(tpe)
	implicit def parserWithSymbolic[A](psr:Parser[A]) = new ParserWithSymbolic[ctx.type, A](psr)
	implicit def str2parserWithSymbolic(str:String) = this.parserWithSymbolic(this.str2parser(str))
	implicit def type2parserWithSymbolic[A](tpe:ctx.TypeTag[A]) = this.parserWithSymbolic(this.OfType(tpe))
}

/** Adds symbolic methods to Parsers */
class ParserWithSymbolic[U <: Context with Singleton, A](val backing:Parser[U, A]) extends AnyVal {
	def ~[B, Z](rhs:Parser[U, B])(implicit ev:Implicits.AndThenTypes[A,B,Z]) = backing.andThen(rhs)(ev)
	def |[Z >: A](rhs:Parser[U, Z]) = backing.orElse(rhs)
	def rep[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[A, Z]) = backing.repeat(min, max)(ev)
	def opt[Z](implicit ev:Implicits.OptionallyTypes[A, Z]) = backing.optionally(ev)
}

object MacroImpl {
	/** Represents a base-ten digit. */
	private[this] final class Digit(val value:Int)
	private[this] final class Digits(val value:Int)

	private[this] implicit object DigitRepeatTypes extends Implicits.RepeatTypes[Digit, Digits] {
		class Box(var value:Int)
		type Acc = Box
		def init():Acc = new Box(0)
		def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
		def result(acc:Acc):Digits = new Digits(acc.value)
	}

	private[this] trait Parsers extends scpcParsers with ParsersImplictly {
		val IsDigit:Parser[Digit] = CharIn('0' to '9').map(x => new Digit(x - '0'))

		def Int2Digits(min:Int, max:Int) = (IsDigit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, String.format(""""$1%02d" - "$2%02d"""", Integer.valueOf(min), Integer.valueOf(max)))

		def YearP:Parser[ctx.Expr[Year]] = {
			val LiteralP:Parser[ctx.Expr[Year]] = {
				(CharIn("-+").opt ~ IsDigit.rep(1, 9).map(_.value))
					.map({x => if (x._1 == Some('-')) {-x._2} else {x._2}})
					.opaque("\"-999999999\"-\"999999999\"")
					.map(x =>
						{
							val xExpr = ctx.Expr[Int](ctx.universe.Literal(ctx.universe.Constant(x)))
							ctx.universe.reify(java.time.Year.of(xExpr.splice))
						}
					)
			}
			val VariableP:Parser[ctx.Expr[Year]] = OfType(ctx.typeTag[Year])
			VariableP | LiteralP
		}

		def MonthP:Parser[ctx.Expr[Month]] = {
			def monthOfTree(name:String):ctx.Expr[Month] = {
				ctx.Expr(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Select(
								ctx.universe.Ident(
									MacroCompat.newTermName(ctx)("java")
								),
								MacroCompat.newTermName(ctx)("time")
							),
							MacroCompat.newTermName(ctx)("Month")
						),
						MacroCompat.newTermName(ctx)(name)
					)
				)
			}
			val LiteralP:Parser[ctx.Expr[Month]] = {
				Int2Digits(1, 12)
					.map(Month.of _)
					.map(_.name)
					.map(monthOfTree _)
			}
			val VariableP:Parser[ctx.Expr[Month]] = OfType(ctx.typeTag[Month])
			VariableP | LiteralP
		}

		def Day31P:Parser[ctx.Expr[Int]] = {
			val LiteralP:Parser[ctx.Expr[Int]] = {
				Int2Digits(1, 31)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def YearMonthP:Parser[ctx.Expr[YearMonth]] = {
			val PartsP:Parser[ctx.Expr[YearMonth]] = (YearP ~ "-" ~ MonthP).map(x => {
				val (y, m) = x
				ctx.universe.reify(y.splice.atMonth(m.splice))
			})
			val VariableP:Parser[ctx.Expr[YearMonth]] = OfType(ctx.typeTag[YearMonth])
			VariableP | PartsP
		}

		def LocalDateP:Parser[ctx.Expr[LocalDate]] = {
			val YearMonthVariantP:Parser[ctx.Expr[LocalDate]] = (YearMonthP ~ "-" ~ Day31P).map(x => {
				val (ym, day) = x
				ctx.universe.reify(ym.splice.atDay(day.splice))
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
			val LiteralP:Parser[ctx.Expr[LocalTime]] = (HourP ~ ":" ~ MinuteP ~ (":" ~ SecondP ~ ("." ~ NanoP).opt).opt)
				.map({hmsn =>
					val constZero = ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(0)))
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((constZero, None))
					val nano = n.getOrElse(constZero)

					ctx.universe.reify(java.time.LocalTime.of(hour.splice, minute.splice, second.splice, nano.splice))
				})
			val VariableP:Parser[ctx.Expr[LocalTime]] = OfType(ctx.typeTag[LocalTime])
			VariableP | LiteralP
		}

		def LocalDateTimeP:Parser[ctx.Expr[LocalDateTime]] = {
			(LocalDateP ~ "T" ~ LocalTimeP)
				.map({dt =>
					val (date, time) = dt
					ctx.universe.reify(date.splice.atTime(time.splice))
				})
		}
	}

	private[this] val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

	def stringContext_localdate(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDate] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localtime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalTimeP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localdatetime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateTimeP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}
}
