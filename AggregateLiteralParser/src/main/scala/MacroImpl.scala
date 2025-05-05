package name.rayrobdod.stringContextParserCombinatorExample.aggregateLiteral

import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Interpolator => _, *}
import name.rayrobdod.stringContextParserCombinator.Interpolator.quotedInterpolators.*

enum Sign {
	case Positive
	case Negative

	def *(x:Int):Int = {
		this match {
			case Positive => x
			case Negative => -x
		}
	}
}

object MacroImpl {
	private val whitespace: Interpolator[Unit] = charIn(" \t\r\n").repeat().void.hide

	private val int: Interpolator[Expr[Int]] = {
		val sign = isString("-").optionally()(using new typeclass.Optionally[Any, Unit, Sign] {
			def none(implicit ctx: Any):Sign = Sign.Positive
			def some(elem:Unit)(implicit ctx: Any):Sign = Sign.Negative
		})

		val digits = charIn("0").map(_ => 0) <|>
			(charIn('1' to '9') <~> charIn(('0' to '9') :+ '_').repeat())
				.map({(ht) =>
					val (h, t2) = ht
					val t = t2.filter(_ != '_')
					s"$h$t".toInt
				})

		(sign <~> digits).map((sd) => sd._1 * sd._2).mapToExpr <~ whitespace
	}

	private val string: Interpolator[Expr[String]] = {
		def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] =
			charWhere((x) => pf.isDefinedAt(x)).flatMap((x) => pf(x))

		val charImmediate = charWhere(x => x != '\"' && x != '\\' && x != '\n' && x != '\r')
		val charEscaped = (
			(isString("\\") ~> charFlatCollect({
				case '\\' => pass.map(_ => '\\')
				case '"' => pass.map(_ => '"')
				case '\'' => pass.map(_ => '\'')
				case 'n' => pass.map(_ => '\n')
				case 'r' => pass.map(_ => '\r')
				case 'b' => pass.map(_ => '\b')
				case 'f' => pass.map(_ => '\f')
				case 't' => pass.map(_ => '\t')
				case 'u' => charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(4,4).map(x => Integer.parseInt(x, 16).toChar)
			}))
		)

		isString("\"") ~> (charImmediate <|> charEscaped).repeat().mapToExpr <~ isString("\"") <~ whitespace
	}

	private def caseClassField(quotes: Quotes)(symbolA: quotes.reflect.Symbol): Interpolator[Expr[?]] = {
		(isString(symbolA.name) ~> whitespace ~> isString("=") ~> whitespace).optionally() ~> obj(quotes)(symbolA.typeRef.baseClasses(0).typeRef)
	}

	private def caseClass[Z](quotes: Quotes)(tpeZ: quotes.reflect.TypeRepr): Interpolator[Expr[Z]] = {
		import quotes.reflect.*
		val symbolZ = tpeZ.typeSymbol

		val paramParsers =
			symbolZ.caseFields
				.map(caseClassField(quotes))
				.reverse

		val paramsParser =
			paramParsers.tail
				.foldLeft[Interpolator[List[Expr[?]]]](paramParsers.head.map(x => List(x)))({(folding, value) =>
					(value <~ isString(",") <~ whitespace) `<::>` folding
				}).map(params =>
					New(TypeIdent(symbolZ)).select(symbolZ.primaryConstructor).appliedToArgs(params.map(_.asTerm)).asExpr.asInstanceOf[Expr[Z]]
				)

		isString("[") ~> whitespace ~> paramsParser <~ whitespace <~ isString("]") <~ whitespace
	}


	private def obj[Z](quotes: Quotes)(tpeZ: quotes.reflect.TypeRepr): Interpolator[Expr[Z]] = {
		import quotes.reflect.*
		given Quotes = quotes

		val interpolation = (
			ofType[Z](using new {
				def createType(using Quotes): Type[Z] = tpeZ.asType.asInstanceOf[Type[Z]]
			})
		)

		val literal = (
			if (tpeZ <:< TypeRepr.of[Int]) {
				Option(int.asInstanceOf[Interpolator[Expr[Z]]])
			} else
			if (tpeZ <:< TypeRepr.of[String]) {
				Option(string.asInstanceOf[Interpolator[Expr[Z]]])
			} else
			if (tpeZ.typeSymbol.caseFields.nonEmpty) {
				Option(caseClass(quotes)(tpeZ))
			} else
			{
				println(Printer.TypeReprCode.show(tpeZ))
				Option.empty
			}
		)

		literal.map(_ <|> interpolation).getOrElse(interpolation)
	}

	def stringContext_cc[Z](sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Type[Z], Quotes):Expr[Z] = {
		val retval = (whitespace ~> obj[Z](quotes)(quotes.reflect.TypeRepr.of[Z]) <~ end).interpolate(sc, args)
		println(retval.show)
		retval
	}
}
