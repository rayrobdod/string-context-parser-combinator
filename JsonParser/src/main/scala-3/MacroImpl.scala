package name.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.Seq
import scala.math.BigDecimal
import scala.quoted.{Expr, Quotes, Type}
import org.json4s._
import name.rayrobdod.stringContextParserCombinator._

object MacroImpl {
	import scala.language.higherKinds
	private def myLiftFunction[Z : Type, Lifter[A] <: Lift[A, Z] : Type]:LiftFunction[Lifter, Expr[Z]] = {
		new LiftFunction[Lifter, Expr[Z]] {
			def apply[A](lifter:Expr[Lifter[A]], a:Expr[A])(using Type[A], Quotes):Expr[Z] = lifter match {
				// Lifter being a Lift.jvalue implies that A =:= Z
				case '{ Lift.jvalue } => a.asExprOf[Z]
				case _ => '{ $lifter.apply($a) }
			}
		}
	}

	/**
	 * A micro-optimization:
	 * Since the `Lift.String` instances create a `JString`
	 * (so that they work in general contexts that want a JValue),
	 * naive use in places that want a `String` (such as map keys and string concatenation)
	 * would be prone to generating code that wraps and then immediately unwraps a String
	 * (i.e. `Lift.string.apply("abcd").values`).
	 * This will bypass that wrapping, at least for the built-in `Lift.string`.
	 */
	private object stringLiftFunction extends LiftFunction[Lift.String, Expr[String]] {
		def apply[A](lifter:Expr[Lift.String[A]], a:Expr[A])(using Type[A], Quotes):Expr[String] = lifter match {
			// Lifter being a Lift.jvalue implies that A =:= Z, and for Lift.String, Z =:= JString
			case '{ Lift.jvalue } => '{ ${a.asExprOf[JString]}.values }
			// Lifter being a Lift.string implies that A =:= String
			case '{ Lift.string } => a.asExprOf[String]
			case _ => '{ $lifter.apply($a).values }
		}
	}

	import name.rayrobdod.stringContextParserCombinator.Parser._
	import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	import name.rayrobdod.stringContextParserCombinator.Extractor.Extractor

	private def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] =
		charWhere(pf.isDefinedAt).flatMap(pf.apply)

	private def whitespace(using Quotes):Parser[Unit] = {
		charIn("\n\r\t ")
			.void
			.repeat(strategy = RepeatStrategy.Possessive)
			.hide
	}

	private def jnull(using Quotes):Parser[Expr[JNull.type]] = {
		isString("null")
			.map(_ => '{ _root_.org.json4s.JsonAST.JNull })
			.extractorAtom
	}

	private def jboolean(using Quotes):Parser[Expr[JBool]] = {
		val jTrue = isString("true")
			.map(_ => '{ _root_.org.json4s.JsonAST.JBool.True })
			.extractorAtom[Expr, Type, JBool]
		val jFalse = isString("false")
			.map(_ => '{ _root_.org.json4s.JsonAST.JBool.False })
			.extractorAtom[Expr, Type, JBool]
		jTrue <|> jFalse
	}

	private def jnumber(using Quotes):Parser[Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper
		import Interpolator.charIn

		def repeatedDigits(min:Int):Interpolator[String] = charIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		given typeclass.Sequenced[String, String, String] with {
			def aggregate(a:String, b:String):String = a + b
		}
		given typeclass.Optionally[Char, String] = typeclass.Optionally("", _.toString)
		given typeclass.Optionally[String, String] = typeclass.Optionally.whereDefault[String]("")

		val stringRepr: Interpolator[String] = (
			charIn("-").optionally()
			<~> (charIn("0").map(_.toString) <|> (charIn('1' to '9').map(_.toString) <~> repeatedDigits(0)))
			<~> (charIn(".").map(_.toString) <~> repeatedDigits(1)).optionally()
			<~> (charIn("eE").map(_.toString) <~> charIn("+-").optionally() <~> repeatedDigits(1)).optionally()
		)
			.opaque("Number Literal")

		val interpolator = stringRepr.map({s => '{JDecimal(${Expr(BigDecimal(s))})}})

		val extractor = stringRepr
			.map:
				s => Expr(BigDecimal(s))
			.extractorAtom[Expr, Type, BigDecimal]
			.toExtractor
			.contramap[Expr[JValue & JNumber]]:
				n => '{jnumber2bigdecimal($n)}

		paired(interpolator, extractor)
	}

	def jnumber2bigdecimal(n: JNumber): BigDecimal = {
		n match {
			case JDecimal(bd) => bd
			case JDouble(d) => d
			case JInt(bi) => BigDecimal(bi)
			case JLong(i) => i
		}
	}

	private def stringBase(using Quotes):Parser[Expr[String]] = {
		import Interpolator._
		val delimiter:Parser[Unit] = Parser.isString("\"")
		val jCharImmediate:Interpolator[Char] = charWhere(c => c >= ' ' && c != '"' && c != '\\').opaque("printable character other than '\"' or '\\'")
		val jCharEscaped:Interpolator[Char] = (
			(isString("\\") ~> charFlatCollect({
				case '\\' => pass.map(_ => '\\')
				case '/' => pass.map(_ => '/')
				case '"' => pass.map(_ => '"')
				case 'n' => pass.map(_ => '\n')
				case 'r' => pass.map(_ => '\r')
				case 'b' => pass.map(_ => '\b')
				case 'f' => pass.map(_ => '\f')
				case 't' => pass.map(_ => '\t')
				case 'u' => charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(4,4).map(x => Integer.parseInt(x, 16).toChar)
			}))
		)
		val jChar:Interpolator[Char] = jCharEscaped <|> jCharImmediate
		val jCharsImmediate:Parser[Expr[String]] = jChar
			.repeat(1, strategy = RepeatStrategy.Possessive)
			.mapToExpr
			.extractorAtom

		val jCharsLifted:Parser[Expr[String]] = paired(
			lifted[Lift.String, Expr[String]](
				stringLiftFunction,
				"A for Lift[A, JString]"
			),
			Extractor.ofType[String],
		)
		val content:Parser[Expr[String]] = paired(
			(jCharsLifted <|> jCharsImmediate)
				.toInterpolator
				.repeat(strategy = RepeatStrategy.Possessive)(using typeclass.Repeated.quotedConcatenateString)
			,
			(jCharsImmediate).toExtractor
		)
		(delimiter ~> content <~ delimiter)
	}

	private def jstring(using Quotes):Parser[Expr[JString]] = {
		stringBase.imap(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)}, x => '{$x.values})
	}

	private def jarray(using Quotes):Parser[Expr[JArray]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("[") <~> whitespace
		val delimiter:Parser[Unit] = isString(",") <~> whitespace
		val suffix:Parser[Unit] = isString("]")

		val interpolator:Interpolator[Expr[JArray]] = {
			val liftedArray = Interpolator.lifted[Lift.Array, Expr[JArray]](
				myLiftFunction[JArray, Lift.Array],
				"A for Lift[A, JArray]"
			)
				.map(x => '{ $x.arr })
				.andThen(whitespace.toInterpolator)

			val splicableValues:Interpolator[typeclass.Repeated.SplicePiece[Expr,JValue]] = {
				given typeclass.Eithered[Expr[JValue], Expr[List[JValue]], typeclass.Repeated.SplicePiece[Expr,JValue]] = typeclass.Eithered.quotedSplicePiece

				val value = jvalue.toInterpolator
				val array = isString("..").toInterpolator ~> liftedArray <~ whitespace.toInterpolator
				value <|> array
			}

			splicableValues
				.repeat(
					delimiter = delimiter.toInterpolator,
					strategy = RepeatStrategy.Possessive)(
					typeclass.Repeated.quotedFromSplicesToExprList
				)
				.map(x => '{ JArray.apply($x)})
		}

		val extractor:Extractor[Expr[JArray]] = {
			jvalue.toExtractor
				.repeat(delimiter = delimiter.toExtractor)
				.contramap((x:Expr[JArray]) => '{ $x.arr })
		}

		prefix ~> paired(interpolator, extractor) <~ suffix
	})

	private def jobject(using Quotes):Parser[Expr[JObject]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("{") <~> whitespace
		val separator:Parser[Unit] = isString(":") <~> whitespace
		val delimiter:Parser[Unit] = isString(",") <~> whitespace
		val suffix:Parser[Unit] = isString("}")

		val interpolator:Interpolator[Expr[JObject]] = {
			val liftedObject = Interpolator.lifted[Lift.Object, Expr[JObject]](
				myLiftFunction[JObject, Lift.Object],
				"A for Lift[A, JObject]"
			).map(x => '{ $x.obj })

			val liftedKeyValue = Interpolator.lifted[Lift.KeyValue, Expr[(java.lang.String, JValue)]](
				myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
				"A for Lift[A, (String, JValue)]"
			)

			val key = {
				val jCharsLifted:Interpolator[Expr[String]] = Interpolator.lifted[Lift.String, Expr[String]](
					stringLiftFunction,
					"A for Lift[A, JString]"
				)
				val immediate:Interpolator[Expr[String]] = stringBase.toInterpolator

				(jCharsLifted <|> immediate) <~> whitespace.toInterpolator
			}

			val splicableValues:Interpolator[typeclass.Repeated.SplicePiece[Expr,(String, JValue)]] = {
				given typeclass.Eithered[Expr[(String, JValue)], Expr[List[(String, JValue)]], typeclass.Repeated.SplicePiece[Expr,(String, JValue)]] = typeclass.Eithered.quotedSplicePiece

				val keyValue = (liftedKeyValue <~> whitespace.toInterpolator)
				val keyThenValue = (key <~> separator.toInterpolator <~> jvalue.toInterpolator)
					.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
				val splice = (isString("..").toInterpolator ~> liftedObject <~> whitespace.toInterpolator)
				(keyValue <|> keyThenValue) <|> splice
			}

			splicableValues
				.repeat(
					delimiter = delimiter.toInterpolator,
					strategy = RepeatStrategy.Possessive)(
					typeclass.Repeated.quotedFromSplicesToExprList
				)
				.map(x => '{ JObject.apply($x)})
		}

		val extractor:Extractor[Expr[JObject]] = {
			ofType[(String, JValue)].toExtractor
				.repeat(delimiter = delimiter.toExtractor)
				.contramap((x:Expr[JObject]) => '{ $x.obj })
		}

		prefix ~> paired(interpolator, extractor) <~ suffix
	})

	private def jlifted(using Quotes):Parser[Expr[JValue]] = paired(
		Interpolator.lifted[Lift.Value, Expr[JValue]](
			myLiftFunction[JValue, Lift.Value],
			"Liftable Value"
		),
		Extractor.ofType[JValue]
	)

	private def jvalue(using Quotes):Parser[Expr[JValue]] = {
		extension [A <: JValue](parser:Parser[Expr[A]])
			def widenToJValue(using Type[A]):Parser[Expr[JValue]] = {
				parser.widenWith(
					Predef.identity,
					PartialExprFunction(
						(value) => '{$value.isInstanceOf[A]},
						(value) => '{$value.asInstanceOf[A]},
					),
				)
			}

		((
			jnull.widenToJValue <|>
			jboolean.widenToJValue <|>
			jnumber.widenToJValue <|>
			jstring.widenToJValue <|>
			jarray.widenToJValue <|>
			jobject.widenToJValue <|>
			jlifted
		) <~ whitespace)
	}

	private def onlyJvalue(using Quotes) = (whitespace <~> jvalue <~> end)

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		onlyJvalue.interpolate(sc, args)
	}

	def stringContext_json_unapply(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[JValue]] = {
		onlyJvalue.extractor(sc)
	}
}
