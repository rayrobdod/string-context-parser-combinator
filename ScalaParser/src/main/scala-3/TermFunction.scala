package name.rayrobdod.stringContextParserCombinatorExample.quasiquotes

import scala.quoted.*

trait TermFunction:
	def apply()(using quotes: Quotes): quotes.reflect.Term

object TermFunction:
	final class FromExpr(expr: Expr[?]) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			import quotes.reflect.asTerm
			expr.asTerm

	final class Lifted[A](lifter: ToExpr[A], a: A) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			import quotes.reflect.asTerm
			lifter(a).asTerm

	final class NullConstant extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.NullConstant())

	final class UnitConstant extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.UnitConstant())

	final class BooleanConstant(v: Boolean) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.BooleanConstant(v))

	final class IntConstant(v: Int) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.IntConstant(v))

	final class LongConstant(v: Long) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.LongConstant(v))

	final class CharConstant(v: Char) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.CharConstant(v))

	final class StringConstant(v: String) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.Literal(quotes.reflect.StringConstant(v))

	final class If(cond: TermFunction, thenp: TermFunction, elsep: TermFunction) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			quotes.reflect.If(cond(), thenp(), elsep())

	final class SelectUnary(targetFn: TermFunction, name: String) extends TermFunction:
		def apply()(using quotes: Quotes): quotes.reflect.Term =
			val target = targetFn()
			val sym = target.tpe.typeSymbol.methodMember(name).filter(_.paramSymss.isEmpty)
			if sym.isEmpty then
				quotes.reflect.report.errorAndAbort(s"No such method: ${target.show}.${name}")
			target.select(sym.head)
