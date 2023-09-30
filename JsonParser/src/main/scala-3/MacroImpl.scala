package name.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.Seq
import scala.quoted.{Expr, Quotes, Type}
import org.json4s.{JValue, JNull, JBool, JNumber, JString, JArray, JObject}
import name.rayrobdod.stringContextParserCombinator._

object MacroImpl {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	private def concatenateStrings(strings:Seq[Expr[String]])(using Quotes):Expr[String] = {
		strings match {
			case Seq() => '{ "" }
			case Seq(x) => x
			case _ => '{
				${strings.foldLeft('{new _root_.java.lang.StringBuilder()})({(builder, part) => '{$builder.append($part)}})}.toString
			}
		}
	}

	private def assembleCollection[A]
			(parts:List[Either[Expr[A], Expr[List[A]]]])
			(using Type[A], Quotes)
	:Expr[List[A]] = {
		val builder = parts.foldLeft('{ List.newBuilder[A] })({(builder, part) =>
			part match {
				case Left(single) => '{ $builder.addOne($single) }
				case Right(group) => '{ $builder.addAll($group) }
			}
		})
		'{ $builder.result() }
	}

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
	 * A micro-optimization; particularly useful since Lifted lifts to a JString and Map keys and
	 * string building take java Strings; the macro is particularly likely to wrap a java String
	 * and then immediately unwrap that string.
	 *
	 * that is, generate something like `Lift.string.apply("abcd").values`
	 */
	private def jstringExprToStringExpr(using Quotes)(in:Expr[JString]):Expr[String] = in match {
		case '{ name.rayrobdod.stringContextParserCombinatorExample.json.Lift.string.apply(${param}) } => param
		case '{ name.rayrobdod.stringContextParserCombinatorExample.json.Lift.jvalue[JString].apply(${param}) } => '{ $param.values }
		case _ => '{ $in.values }
	}

	import name.rayrobdod.stringContextParserCombinator.Parser._
	import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	import name.rayrobdod.stringContextParserCombinator.Extractor.Extractor

	private def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] =
		charWhere(pf.isDefinedAt).flatMap(pf.apply)

	private def whitespace(using Quotes):Parser[Unit] = {
		charIn("\n\r\t ")
			.imap(_ => (), _ => ' ')
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
		jTrue orElse jFalse
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

		(
			charIn("-").optionally()
			andThen (charIn("0").map(_.toString) orElse (charIn('1' to '9').map(_.toString) andThen repeatedDigits(0)))
			andThen (charIn(".").map(_.toString) andThen repeatedDigits(1)).optionally()
			andThen (charIn("eE").map(_.toString) andThen charIn("+-").optionally() andThen repeatedDigits(1)).optionally()
		)
			.map({x =>
				'{ _root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply( ${Expr[String](x)} )) }
			})
			.opaque("Number Literal")
			.extractorAtom
	}

	private def stringBase(using Quotes):Parser[Expr[String]] = {
		import Interpolator._
		val delimiter:Parser[Unit] = Parser.isString("\"")
		val jCharImmediate:Interpolator[Char] = charWhere(c => c >= ' ' && c != '"' && c != '\\').opaque("printable character other than '\"' or '\\'")
		val jCharEscaped:Interpolator[Char] = (
			(isString("\\") andThen charFlatCollect({
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
		val jChar:Interpolator[Char] = jCharEscaped orElse jCharImmediate
		val jCharsImmediate:Parser[Expr[String]] = jChar
			.repeat(1, strategy = RepeatStrategy.Possessive)
			.mapToExpr
			.extractorAtom

		val jCharsLifted:Parser[Expr[String]] = paired(
			lifted[Lift.String, Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(jstringExprToStringExpr _),
			Extractor.ofType[String],
		)
		val content:Parser[Expr[String]] = paired(
			(jCharsLifted orElse jCharsImmediate)
				.toInterpolator
				.repeat(strategy = RepeatStrategy.Possessive)
				.map(strs => concatenateStrings(strs))
			,
			(jCharsImmediate).toExtractor
		)
		(delimiter andThen content andThen delimiter)
	}

	private def jstring(using Quotes):Parser[Expr[JString]] = {
		stringBase.imap(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)}, x => '{$x.values})
	}

	private def jarray(using Quotes):Parser[Expr[JArray]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("[") andThen whitespace
		val delimiter:Parser[Unit] = isString(",") andThen whitespace
		val suffix:Parser[Unit] = isString("]")

		val interpolator:Interpolator[Expr[JArray]] = {
			val liftedArray = Interpolator.lifted[Lift.Array, Expr[JArray]](
				myLiftFunction[JArray, Lift.Array],
				"A for Lift[A, JArray]"
			)
				.map(x => '{ $x.arr })
				.andThen(whitespace.toInterpolator)

			val splicableValue:Interpolator[Either[Expr[JValue], Expr[List[JValue]]]] = (
				jvalue.map(x => Left(x)) orElse
				(
					isString("..").toInterpolator
					andThen liftedArray
					andThen whitespace.toInterpolator
					).map(x => Right(x))
			)

			val literalPresplice:Interpolator[List[Either[Expr[JValue], Expr[List[JValue]]]]] = (
				splicableValue.repeat(delimiter = delimiter.toInterpolator, strategy = RepeatStrategy.Possessive)
			)

			literalPresplice
				.map(xs => assembleCollection(xs))
				.map(x => '{ JArray.apply($x)})
		}

		val extractor:Extractor[Expr[JArray]] = {
			jvalue.toExtractor
				.repeat(delimiter = delimiter.toExtractor)
				.contramap((x:Expr[JArray]) => '{ $x.arr })
		}

		prefix andThen paired(interpolator, extractor) andThen suffix
	})

	private def jobject(using Quotes):Parser[Expr[JObject]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("{") andThen whitespace
		val separator:Parser[Unit] = isString(":") andThen whitespace
		val delimiter:Parser[Unit] = isString(",") andThen whitespace
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
				val jCharsLifted:Interpolator[Expr[String]] = Interpolator.lifted[Lift.String, Expr[JString]](
					myLiftFunction[JString, Lift.String],
					"A for Lift[A, JString]"
				).map(jstringExprToStringExpr _)
				val immediate:Interpolator[Expr[String]] = stringBase.toInterpolator

				(jCharsLifted orElse immediate) andThen whitespace.toInterpolator
			}

			val splicableValue:Interpolator[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]] = (
				(liftedKeyValue andThen whitespace.toInterpolator)
					.map(x => Left(x)) orElse
				(key andThen separator.toInterpolator andThen jvalue.toInterpolator)
					.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
					.map(x => Left(x)) orElse
				(isString("..").toInterpolator andThen liftedObject andThen whitespace.toInterpolator)
					.map(x => Right(x))
			)

			val literalPresplice:Interpolator[List[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]]] = (
				splicableValue.repeat(delimiter = delimiter.toInterpolator, strategy = RepeatStrategy.Possessive)
			)

			literalPresplice
				.map(xs => assembleCollection(xs))
				.map(x => '{ JObject.apply($x)})
		}

		val extractor:Extractor[Expr[JObject]] = {
			ofType[(String, JValue)].toExtractor
				.repeat(delimiter = delimiter.toExtractor)
				.contramap((x:Expr[JObject]) => '{ $x.obj })
		}

		prefix andThen paired(interpolator, extractor) andThen suffix
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
			jnull.widenToJValue orElse
			jboolean.widenToJValue orElse
			jnumber.widenToJValue orElse
			jstring.widenToJValue orElse
			jarray.widenToJValue orElse
			jobject.widenToJValue orElse
			jlifted
		) andThen whitespace)
	}

	private def onlyJvalue(using Quotes) = (whitespace andThen jvalue andThen end)

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		onlyJvalue.interpolate(sc, args)
	}

	def stringContext_json_unapply(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[JValue]] = {
		onlyJvalue.extractor(sc)
	}
}
