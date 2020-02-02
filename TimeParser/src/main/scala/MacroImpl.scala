package com.rayrobdod.stringContextParserCombinator
package example.datetime

import java.time._
import com.rayrobdod.stringContextParserCombinator.{Parsers => scpcParsers}
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context
import com.rayrobdod.stringContextParserCombinator.Utilities._
import scala.Predef.charWrapper

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
		type ContextType <: Context with Singleton
		val ctx:ContextType

		val IsDigit:Parser[Digit] = CharIn('0' to '9').map(x => new Digit(x - '0'))

		def Int2Digits(min:Int, max:Int) = (IsDigit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, String.format(""""$1%02d" - "$2%02d"""", Integer.valueOf(min), Integer.valueOf(max)))

		def YearP:Parser[ContextType#Expr[Year]] = {
			val LiteralP:Parser[ContextType#Expr[Year]] = {
				(CharIn("-+").opt ~ IsDigit.rep(1, 9).map(_.value))
					.map({x => if (x._1 == Some('-')) {-x._2} else {x._2}})
					.opaque("\"-999999999\"-\"999999999\"")
					.map(x =>
						ctx.Expr[Year](objectApply(ctx)(
							selectChain(ctx, "java.time.Year").apply,
							"of",
							List(ctx.universe.Literal(ctx.universe.Constant(x)))
						))
					)
			}
			val VariableP:Parser[ContextType#Expr[Year]] = OfType(ctx.typeTag[Year])
			VariableP | LiteralP
		}

		def MonthP:Parser[ContextType#Expr[Month]] = {
			def monthOfTree(name:String):ContextType#Expr[Month] = {
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
			val LiteralP:Parser[ContextType#Expr[Month]] = {
				Int2Digits(1, 12)
					.map(Month.of _)
					.map(_.name)
					.map(monthOfTree _)
			}
			val VariableP:Parser[ContextType#Expr[Month]] = OfType(ctx.typeTag[Month])
			VariableP | LiteralP
		}

		def Day31P:Parser[ContextType#Expr[Int]] = {
			val LiteralP:Parser[ContextType#Expr[Int]] = {
				Int2Digits(1, 31)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def YearMonthP:Parser[ContextType#Expr[YearMonth]] = {
			val PartsP:Parser[ContextType#Expr[YearMonth]] = (YearP ~ "-" ~ MonthP).map(x =>
				ctx.Expr[YearMonth](Utilities.objectApply(ctx)(x._1.in(ctx.mirror).tree, "atMonth", List(x._2.in(ctx.mirror).tree)))
			)
			val VariableP:Parser[ContextType#Expr[YearMonth]] = OfType(ctx.typeTag[YearMonth])
			VariableP | PartsP
		}

		def LocalDateP:Parser[ContextType#Expr[LocalDate]] = {
			val YearMonthVariantP:Parser[ContextType#Expr[LocalDate]] = (YearMonthP ~ "-" ~ Day31P).map(x =>
				ctx.Expr[LocalDate](Utilities.objectApply(ctx)(x._1.in(ctx.mirror).tree, "atDay", List(x._2.in(ctx.mirror).tree)))
			)
			val VariableP:Parser[ContextType#Expr[LocalDate]] = OfType(ctx.typeTag[LocalDate])
			VariableP | YearMonthVariantP
		}

		def HourP:Parser[ContextType#Expr[Int]] = {
			val LiteralP:Parser[ContextType#Expr[Int]] = {
				Int2Digits(0, 23)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def MinuteP:Parser[ContextType#Expr[Int]] = {
			val LiteralP:Parser[ContextType#Expr[Int]] = {
				Int2Digits(0, 59)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def SecondP:Parser[ContextType#Expr[Int]] = MinuteP

		def NanoP:Parser[ContextType#Expr[Int]] = {
			val LiteralP = CharIn('0' to '9').rep(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			LiteralP
		}

		def LocalTimeP:Parser[ContextType#Expr[LocalTime]] = {
			val LiteralP = (HourP ~ ":" ~ MinuteP ~ (":" ~ SecondP ~ ("." ~ NanoP).opt).opt)
				.map({hmsn =>
					val constZero = ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(0)))
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((constZero, None))
					val nano = n.getOrElse(constZero)

					ctx.Expr[LocalTime](objectApply(ctx)(
						selectChain(ctx, "java.time.LocalTime").apply,
						"of",
						List(
							hour.in(ctx.mirror).tree,
							minute.in(ctx.mirror).tree,
							second.in(ctx.mirror).tree,
							nano.in(ctx.mirror).tree
						)
					))
				})
			val VariableP:Parser[ContextType#Expr[LocalTime]] = OfType(ctx.typeTag[LocalTime])
			VariableP | LiteralP
		}

		def LocalDateTimeP:Parser[ContextType#Expr[LocalDateTime]] = {
			(LocalDateP ~ "T" ~ LocalTimeP)
				.map({dt =>
					val (date, time) = dt

					ctx.Expr[LocalDateTime](objectApply(ctx)(
						date.in(ctx.mirror).tree,
						"atTime",
						List(time.in(ctx.mirror).tree)
					))
				})
		}
	}

	def stringContext_xxx[A](c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*)(parser:Parser[c.type, A]):A = {
		val self:c.Expr[DateTimeStringContext] = c.prefix

		/* Extract the string context `parts`, to be used in the parse */

		val DateTimeSelectChain = selectChain(c, "com.rayrobdod.stringContextParserCombinator.example.datetime.package.DateTimeStringContext")
		val StringContextApply = stringContextApply(c)

		import c.universe._ // ApplyTag, SelectTag etc.
		val strings = self.tree.duplicate match {
			case c.universe.Apply(
				DateTimeSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (MacroCompat.eval(c)(x), PositionPoint(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(self))
		}

		/* Create the input to parse */

		val input = Input[c.type](strings, args.toList)

		/* Parse the input */

		parser.parse(input) match {
			case Success(res, _) => {
				//System.out.println(res)
				res
			}
			case f:Failure => {
				f.report(c)
			}
		}
	}

	def stringContext_localdate(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDate] = {
		object parsers extends Parsers {
			type ContextType = c.type
			val ctx:ContextType = c

			def Aggregate = (this.LocalDateP ~ this.End())
		}

		this.stringContext_xxx(c)(args:_*)(parsers.Aggregate)
	}

	def stringContext_localtime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalTime] = {
		object parsers extends Parsers {
			type ContextType = c.type
			val ctx:ContextType = c

			def Aggregate = (this.LocalTimeP ~ this.End())
		}

		this.stringContext_xxx(c)(args:_*)(parsers.Aggregate)
	}

	def stringContext_localdatetime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
		object parsers extends Parsers {
			type ContextType = c.type
			val ctx:ContextType = c

			def Aggregate = (this.LocalDateTimeP ~ this.End())
		}

		this.stringContext_xxx(c)(args:_*)(parsers.Aggregate)
	}
}
