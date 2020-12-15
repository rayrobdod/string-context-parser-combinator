package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.language.implicitConversions
import scala.Predef.charWrapper
import scala.quoted._
import com.rayrobdod.stringContextParserCombinator._

given Conversion[String, Parsers.Parser[Unit]] = Parsers.IsString(_)
given [A](using Quotes) as Conversion[Type[A], Parsers.Parser[Expr[A]]] {
	  def apply(typ:Type[A]):Parsers.Parser[Expr[A]] = Parsers.OfType[A](using typ)
}

/** Adds symbolic methods to Parsers */
extension [U, A, B, Z] (backing:Parser[U, A])
	def ~(rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThen(rhs)(ev)
	def ~/(rhs:Parser[U, B])(implicit ev:typelevel.Sequenced[A,B,Z]) = backing.andThenWithCut(rhs)(ev)

extension [U, A, Z >: A] (backing:Parser[U, A])
	def |(rhs:Parser[U, Z]) = backing.orElse(rhs)

extension [U, A, Z] (backing:Parser[U, A])
	def rep(min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:typelevel.Repeated[A, Z]) = backing.repeat(min, max)(ev)
	def opt(implicit ev:typelevel.Optionally[A, Z]) = backing.optionally(ev)


object MacroImpl {
	/** Represents a base-ten digit. */
	private[this] final class Digit(val value:Int)
	private[this] final class Digits(val value:Int)

	private[this] implicit object DigitRepeatTypes extends typelevel.Repeated[Digit, Digits] {
		final class Box(var value:Int)
		type Acc = Box
		def init():Acc = new Box(0)
		def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
		def result(acc:Acc):Digits = new Digits(acc.value)
	}

	import com.rayrobdod.stringContextParserCombinator.Parsers._
	private[this] def IsDigit:Parser[Digit] = CharIn('0' to '9').map(x => new Digit(x - '0'))

	private[this] def Int2Digits(min:Int, max:Int) = (IsDigit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, Expecting(String.format("""%02d <= $value <= %02d""", Integer.valueOf(min), Integer.valueOf(max))))

	private[this] def YearP(using Quotes):Parser[Expr[Year]] = {
		val LiteralP:Parser[Expr[Year]] = {
			(CharIn("-+").opt ~ IsDigit.rep(1, 9).map(_.value))
				.map({x => if (x._1 == Some('-')) {-x._2} else {x._2}})
				.opaque(Expecting("\"-999999999\"-\"999999999\""))
				.map(x =>
					{
						val xExpr = Expr[Int](x)
						'{ java.time.Year.of($xExpr) }
					}
				)
		}
		val VariableP:Parser[Expr[Year]] = OfType[Year]
		VariableP | LiteralP
	}

	private[this] def MonthP(using Quotes):Parser[Expr[Month]] = {
		def monthOfTree(name:String):Expr[Month] = {
			//'{java.time.Month.valueOf(${Expr(name)})}, but using the enum value
			import scala.quoted.quotes.reflect._
			val _root = defn.RootPackage
			val _java = _root.field("java")
			val _time = _java.field("time")
			val _month = _time.field("Month")
			val _instance = _month.field(name)

			Ref(_root).select(_java).select(_time).select(_month).select(_instance).asExprOf[Month]
		}
		val LiteralP:Parser[Expr[Month]] = {
			Int2Digits(1, 12)
				.map(Month.of _)
				.map(_.name)
				.map(monthOfTree _)
		}
		val VariableP:Parser[Expr[Month]] = OfType[Month]
		VariableP | LiteralP
	}

	private[this] def Day31P(using Quotes):Parser[Expr[Int]] = {
		val LiteralP:Parser[Expr[Int]] = {
			Int2Digits(1, 31)
				.map(x => Expr(x))
		}
		LiteralP
	}

	private[this] def YearMonthP(using Quotes):Parser[Expr[YearMonth]] = {
		val PartsP:Parser[Expr[YearMonth]] = (YearP ~ "-" ~/ MonthP).map(x => {
			val (y, m) = x
			'{ $y.atMonth($m) }
		})
		val VariableP:Parser[Expr[YearMonth]] = OfType[YearMonth]
		VariableP | PartsP
	}

	private[this] def LocalDateP(using Quotes):Parser[Expr[LocalDate]] = {
		val YearMonthVariantP:Parser[Expr[LocalDate]] = (YearMonthP ~ "-" ~/ Day31P).map(x => {
			val (ym, day) = x
			'{ $ym.atDay($day) }
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
			.opaque(Expecting("\"0\"-\"999999999\""))
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

				'{ java.time.LocalTime.of($hour, $minute, $second, $nano) }
			})
		val VariableP:Parser[Expr[LocalTime]] = OfType[LocalTime]
		VariableP | LiteralP
	}

	private[this] def LocalDateTimeP(using Quotes):Parser[Expr[LocalDateTime]] = {
		(LocalDateP ~ "T" ~/ LocalTimeP)
			.map({dt =>
				val (date, time) = dt
				'{ $date.atTime($time) }
			})
	}

	def stringContext_localdate(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDate] = {
		val Aggregate = (this.LocalDateP ~ Parsers.End())
		macroimpl[LocalDate](Aggregate)(sc, args)
	}

	def stringContext_localtime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using q:Quotes):Expr[LocalTime] = {
		val Aggregate = (this.LocalTimeP ~ Parsers.End())
		macroimpl[LocalTime](Aggregate)(sc, args)
	}

	def stringContext_localdatetime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDateTime] = {
		val Aggregate = (this.LocalDateTimeP ~ Parsers.End())
		macroimpl[LocalDateTime](Aggregate)(sc, args)
	}
}
