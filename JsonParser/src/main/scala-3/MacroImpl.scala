package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.Seq
import scala.quoted.{Expr, Quotes, Type}
import org.json4s.{JValue, JNull, JBool, JNumber, JLong, JInt, JDecimal, JString, JArray, JObject}
import com.rayrobdod.stringContextParserCombinator._

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
		case '{ com.rayrobdod.stringContextParserCombinatorExample.json.Lift.string.apply(${param}) } => param
		case '{ com.rayrobdod.stringContextParserCombinatorExample.json.Lift.jvalue[JString].apply(${param}) } => '{ $param.values }
		case _ => '{ $in.values }
	}

	import com.rayrobdod.stringContextParserCombinator.Parser._
	import com.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	import com.rayrobdod.stringContextParserCombinator.Extractor.Extractor

	private def CharFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] =
		CharWhere(pf.isDefinedAt).flatMap(pf.apply)

	private def WhitespaceP(using Quotes):Parser[Unit] = {
		CharIn("\n\r\t ")
			.imap(_ => (), _ => ' ')
			.repeat(strategy = RepeatStrategy.Possessive)
	}

	private def NullP(using Quotes):Parser[Expr[JNull.type]] = {
		IsString("null")
			.map(_ => '{ _root_.org.json4s.JsonAST.JNull })
			.extractorAtom
	}

	private def BooleanP(using Quotes):Parser[Expr[JBool]] = {
		val TrueI = IsString("true")
			.map(_ => '{ _root_.org.json4s.JsonAST.JBool.True })
			.extractorAtom[Expr, Type, JBool]
		val FalseI = IsString("false")
			.map(_ => '{ _root_.org.json4s.JsonAST.JBool.False })
			.extractorAtom[Expr, Type, JBool]
		TrueI orElse FalseI
	}

	private def NumberP(using Quotes):Parser[Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper
		import Interpolator.CharIn

		def RepeatedDigits(min:Int):Interpolator[String] = CharIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		given typeclass.Sequenced[String, String, String] with {
			def aggregate(a:String, b:String):String = a + b
		}
		given typeclass.Optionally[Char, String] = typeclass.Optionally("", _.toString)
		given typeclass.Optionally[String, String] = typeclass.Optionally.whereDefault[String]("")

		(
			CharIn("-").optionally()
			andThen (CharIn("0").map(_.toString) orElse (CharIn('1' to '9').map(_.toString) andThen RepeatedDigits(0)))
			andThen (CharIn(".").map(_.toString) andThen RepeatedDigits(1)).optionally()
			andThen (CharIn("eE").map(_.toString) andThen CharIn("+-").optionally() andThen RepeatedDigits(1)).optionally()
		)
			.map({x =>
				'{ _root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply( ${Expr[String](x)} )) }
			})
			.opaque("Number Literal")
			.extractorAtom
	}

	private def StringBase(using Quotes):Parser[Expr[String]] = {
		import Interpolator._
		val DelimiterP:Parser[Unit] = Parser.IsString("\"")
		val JCharImmediate:Interpolator[Char] = CharWhere(c => c >= ' ' && c != '"' && c != '\\').opaque("printable character other than '\"' or '\\'")
		val JCharEscaped:Interpolator[Char] = (
			(IsString("\\") andThen CharFlatCollect({
				case '\\' => Pass.map(_ => '\\')
				case '/' => Pass.map(_ => '/')
				case '"' => Pass.map(_ => '"')
				case 'n' => Pass.map(_ => '\n')
				case 'r' => Pass.map(_ => '\r')
				case 'b' => Pass.map(_ => '\b')
				case 'f' => Pass.map(_ => '\f')
				case 't' => Pass.map(_ => '\t')
				case 'u' => CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(4,4).map(x => Integer.parseInt(x, 16).toChar)
			}))
		)
		val JCharP:Interpolator[Char] = JCharEscaped orElse JCharImmediate
		val JCharsI:Parser[Expr[String]] = JCharP
			.repeat(1, strategy = RepeatStrategy.Possessive)
			.mapToExpr
			.extractorAtom

		val LiftedV:Parser[Expr[String]] = Paired(
			Lifted[Lift.String, Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(jstringExprToStringExpr _),
			Extractor.OfType[String],
		)
		val Content:Parser[Expr[String]] = Paired(
			(LiftedV orElse JCharsI)
				.toInterpolator
				.repeat(strategy = RepeatStrategy.Possessive)
				.map(strs => concatenateStrings(strs))
			,
			(JCharsI).toExtractor
		)
		(DelimiterP andThen Content andThen DelimiterP)
	}

	private def JStringP(using Quotes):Parser[Expr[JString]] = {
		StringBase.imap(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)}, x => '{$x.values})
	}

	private def ArrayP(using Quotes):Parser[Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("[") andThen WhitespaceP
		val Delim:Parser[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Parser[Unit] = IsString("]")

		val LiftedArrayV = Interpolator.Lifted[Lift.Array, Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		)
		val LiftedArrayV2 = LiftedArrayV.map(x => '{ $x.arr })

		val SplicableValue:Interpolator[Either[Expr[JValue], Expr[List[JValue]]]] = (
			ValueP.map(x => Left(x)) orElse
			(
				IsString("..").toInterpolator
				andThen LiftedArrayV2
				andThen WhitespaceP.toInterpolator
				).map(x => Right(x))
		)
		val LiteralPresplice:Interpolator[List[Either[Expr[JValue], Expr[List[JValue]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix.toInterpolator andThen (SplicableValue.repeat(delimiter = Delim.toInterpolator, strategy = RepeatStrategy.Possessive) andThen Suffix.toInterpolator)
		)

		Paired(
			LiteralPresplice
				.map(xs => assembleCollection(xs))
				.map(x => '{ JArray.apply($x)})
			,
			(Prefix andThen (ValueP.repeat(delimiter = Delim)) andThen Suffix)
				.contramap((x:Expr[JArray]) => '{ $x.arr })
		)
	})

	private def ObjectP(using Quotes):Parser[Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("{") andThen WhitespaceP
		val Separator:Parser[Unit] = IsString(":") andThen WhitespaceP
		val Delim:Parser[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Parser[Unit] = IsString("}")

		val ObjectV = Interpolator.Lifted[Lift.Object, Expr[JObject]](
			myLiftFunction[JObject, Lift.Object],
			"A for Lift[A, JObject]"
		)
		val ObjectV2 = ObjectV.map(x => '{ $x.obj })

		val KeyValueV = Interpolator.Lifted[Lift.KeyValue, Expr[(java.lang.String, JValue)]](
			myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
			"A for Lift[A, (String, JValue)]"
		)

		val KeyV = {
			val LiftedV:Interpolator[Expr[String]] = Interpolator.Lifted[Lift.String, Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(jstringExprToStringExpr _)
			val Immediate:Interpolator[Expr[String]] = StringBase.toInterpolator

			(LiftedV orElse Immediate) andThen WhitespaceP.toInterpolator
		}


		val SplicableValue:Interpolator[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]] = (
			(KeyValueV andThen WhitespaceP.toInterpolator)
				.map(x => Left(x)) orElse
			(KeyV andThen Separator.toInterpolator andThen ValueP.toInterpolator)
				.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
				.map(x => Left(x)) orElse
			(IsString("..").toInterpolator andThen ObjectV2 andThen WhitespaceP.toInterpolator)
				.map(x => Right(x))
		)
		val LiteralPresplice:Interpolator[List[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix.toInterpolator andThen (SplicableValue.repeat(delimiter = Delim.toInterpolator, strategy = RepeatStrategy.Possessive) andThen Suffix.toInterpolator)
		)

		Paired(
			LiteralPresplice
				.map(xs => assembleCollection(xs))
				.map(x => '{ JObject.apply($x)})
			,
			(Prefix andThen (OfType[(String, JValue)].repeat(delimiter = Delim)) andThen Suffix)
				.contramap(x => '{ $x.obj })
		)
	})

	private def LiftedP(using Quotes):Parser[Expr[JValue]] = Paired(
		Interpolator.Lifted[Lift.Value, Expr[JValue]](
			myLiftFunction[JValue, Lift.Value],
			"Lifted Value"
		),
		Extractor.OfType[JValue]
	)

	private def ValueP(using Quotes):Parser[Expr[JValue]] = {
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
			NullP.widenToJValue orElse
			BooleanP.widenToJValue orElse
			NumberP.widenToJValue orElse
			JStringP.widenToJValue orElse
			ArrayP.widenToJValue orElse
			ObjectP.widenToJValue orElse
			LiftedP
		) andThen WhitespaceP)
	}

	private def Aggregate(using Quotes) = (WhitespaceP andThen ValueP andThen End)

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		Aggregate.interpolate(sc, args)
	}

	def stringContext_json_unapply(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[JValue]] = {
		Aggregate.extractor(sc)
	}
}
