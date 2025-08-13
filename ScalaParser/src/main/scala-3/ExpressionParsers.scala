package name.rayrobdod.stringContextParserCombinatorExample.quasiquotes

import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.typeclass.*
import name.rayrobdod.stringContextParserCombinator.LiftFunction
import name.rayrobdod.stringContextParserCombinator.RepeatStrategy.Greedy
import name.rayrobdod.stringContextParserCombinator.Interpolator.quotedInterpolators.*
import name.rayrobdod.stringContextParserCombinatorExample.quasiquotes.LexicalParsers.*

object ExpressionParsers:

	def unitOptionally[A](ifAbsent: A, ifPresent: A) = Optionally[Any, Unit, A](_ => ifAbsent, (_,_) => ifPresent)

	extension [A] (p: Interpolator[A]) def collect[Z](pf: PartialFunction[(A, Quotes), Z], description: String): Interpolator[Z] =
		p.filter({(x, quotes) => pf.isDefinedAt((x, quotes))}, description).map({(x, quotes) => pf((x, quotes))})

	private[quasiquotes] def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]): Interpolator[A] =
		charWhere(x => pf.isDefinedAt(x)).flatMap(x => pf(x))


	enum Sign:
		case Positive, Negative
		def *(rhs: BigInt) = if this == Positive then rhs else -rhs

	given given_Sequenced_Sign_BigInt: Sequenced[Any, Sign, BigInt, BigInt] = new:
		def aggregate(sign: Sign, digits: BigInt)(implicit ctx: Any) = sign * digits

	object Sign:
		val minusOpionally: Optionally[Any, Unit, Sign] = unitOptionally(Positive, Negative)



	private[quasiquotes] def digitsWithUnderscores(digit: Interpolator[Char]): Interpolator[String] =
		val digitWithUnderscores: Interpolator[Option[Char]] = digit <|> isString("_")

		given Repeated[Any, Option[Char], String] = new:
			type Acc = StringBuilder
			def init()(implicit ctx: Any) = new StringBuilder
			def append(acc: Acc, elem: Option[Char])(implicit ctx: Any): Acc = {elem.foreach(c => acc += c); acc}
			def result(acc: Acc)(implicit ctx: Any): String = acc.toString

		val middle: Interpolator[String] = digitWithUnderscores.repeat(strategy = Greedy)

		given Sequenced[Any, String, Char, String] = Sequenced((left, right, ctx) => s"$left$right")
		given Sequenced[Any, Char, String, String] = Sequenced((left, right, ctx) => s"$left$right")

		given Optionally[Any, String, String] = Optionally.whereDefault(_ => "")

		digit <~> (middle <~> digit).optionally()
	end digitsWithUnderscores


	private[quasiquotes] val nullLiteral: Interpolator[Expr[TermFunction]] =
		isString("null")
			.map(_ => '{ new TermFunction.NullConstant })

	private[quasiquotes] val booleanLiteral: Interpolator[Expr[TermFunction]] =
		val trueLiteral: Interpolator[Expr[TermFunction]] =
			isString("true")
				.map(_ => '{ new TermFunction.BooleanConstant(true) })
		val falseLiteral: Interpolator[Expr[TermFunction]] =
			isString("false")
				.map(_ => '{ new TermFunction.BooleanConstant(false) })
		trueLiteral <|> falseLiteral

	private[quasiquotes] val integerLiteral: Interpolator[Expr[TermFunction]] =
		given Optionally[Any, BigInt, BigInt] = Optionally.whereDefault(_ => BigInt(0))

		val withLeadingZero = isString("0") <~> charFlatCollect({
			case 'B' | 'b' =>
				digitsWithUnderscores(LexicalParsers.binaryDigit)
					.map(x => BigInt(x, 2))
			case 'X' | 'x' =>
				digitsWithUnderscores(LexicalParsers.hexDigit)
					.map(x => BigInt(x, 16))
			case '0' | '_' =>
				digitsWithUnderscores(LexicalParsers.decimalDigit)
					.map(x => BigInt(x, 10))
			case c if '1' <= c || c <= '9' =>
				digitsWithUnderscores(LexicalParsers.decimalDigit)
					.map(x => BigInt(s"$c$x", 10))
		}).optionally()

		val other = digitsWithUnderscores(LexicalParsers.decimalDigit)
					.map(x => BigInt(x, 10))

		val nonNegativeNumber = withLeadingZero <|> other

		val number = (isString("-").optionally()(using Sign.minusOpionally) <~> nonNegativeNumber)

		val isLong: Interpolator[Boolean] = charIn("lL").void.optionally()(using unitOptionally(false, true))

		(number <~> isLong).collect(
			{
				case ((bi, true), q) if bi.isValidLong =>
					given Quotes = q
					val i: Long = bi.toLong
					val ix: Expr[Long] = Expr(i)
					'{ new TermFunction.LongConstant($ix) }
				case ((bi, false), q) if bi.isValidInt =>
					given Quotes = q
					val i: Int = bi.toInt
					val ix: Expr[Int] = Expr(i)
					'{ new TermFunction.IntConstant($ix) }
			},
			"number out of range"
		)
	end integerLiteral

	private[quasiquotes] val charLiteral: Interpolator[Expr[TermFunction]] =
		val delim = isString("\'")
		val charNoQuoteOrNewline = charWhere(c => c != '\n' && c != '\r' && c != '\'' && c != '\\')
		val element = (charNoQuoteOrNewline <|> escapeSeq)
				.map(c => '{ new TermFunction.CharConstant(${Expr(c)}) })

		delim ~> element <~ delim
	end charLiteral

	private[quasiquotes] val Literal: Interpolator[Expr[TermFunction]] =
		nullLiteral
			<|> booleanLiteral
			<|> charLiteral
			<|> integerLiteral

	private[quasiquotes] val spliceExpr: Interpolator[Expr[TermFunction]] =
		ofType[Expr[?]].map(x => '{ TermFunction.FromExpr($x) })

	private[quasiquotes] val liftedExpr: Interpolator[Expr[TermFunction]] =
		lifted[ToExpr, Expr[TermFunction]](
			new LiftFunction[ToExpr, Expr[TermFunction]]:
				def apply[A](lifter: Expr[ToExpr[A]], a: Expr[A])(using Type[A], Quotes): Expr[TermFunction] =
					'{ TermFunction.Lifted[A]($lifter, $a) }
			,
			"ToExprable"
		)

	private[quasiquotes] val SimpleExpr1: Interpolator[Expr[TermFunction]] =
		spliceExpr
			<|> liftedExpr
			<|> Literal

	private[quasiquotes] val SimpleExpr2: Interpolator[Expr[TermFunction]] =
		(SimpleExpr1 <~> (isString(".") <~> id).repeat()).map: x =>
			x._2.foldLeft(x._1)({(term, name) => '{ TermFunction.SelectUnary($term, ${Expr(name)})}})

	private[quasiquotes] val SimpleExpr: Interpolator[Expr[TermFunction]] =
		SimpleExpr2

	private[quasiquotes] val PrefixExpr: Interpolator[Expr[TermFunction]] =
		(charIn("-+~!").optionally() <~> whitespaces <~> SimpleExpr).map: value =>
			val (opOp, expr) = value
			opOp match
				case None => expr
				case Some(op) => '{ TermFunction.SelectUnary($expr, ${Expr(s"unary_$op")}) }
	end PrefixExpr

	private[quasiquotes] val expr: Interpolator[Expr[TermFunction]] =
		PrefixExpr


	def main(
		sc: Expr[scala.StringContext],
		args: Expr[Seq[Any]],
		quotes1: Expr[Quotes],
		)(
		using Quotes,
	): Expr[Expr[?]] =
		val interpolated: Expr[TermFunction] = (expr <~> end).interpolate(sc, args)

		'{
			val a: TermFunction = $interpolated
			val quotes1Up: Quotes = $quotes1
			val b: quotes1Up.reflect.Term = a(using quotes1Up)
			val c = b.asExpr
			c
		}
	end main
end ExpressionParsers
