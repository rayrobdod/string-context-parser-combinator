package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
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
			(parts:List[Either[Expr[A], Expr[TraversableOnce[A]]]])
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

	import com.rayrobdod.stringContextParserCombinator.Parsers._
	private val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat(strategy = RepeatStrategy.Possessive).map(_ => ())

	private def NullP(using Quotes):Parser[Expr[JNull.type]] = IsString("null").map(_ => '{ _root_.org.json4s.JsonAST.JNull })

	private def BooleanP(using Quotes):Parser[Expr[JBool]] = {
		val TrueI = IsString("true").map(_ => '{ _root_.org.json4s.JsonAST.JBool.True })
		val FalseI = IsString("false").map(_ => '{ _root_.org.json4s.JsonAST.JBool.False })
		TrueI orElse FalseI
	}

	private def NumberP(using Quotes):Parser[Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper

		def RepeatedDigits(min:Int):Parser[String] = CharIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		implicit object StringStringSequentially extends typelevel.Sequenced[String, String, String] {
			def aggregate(a:String, b:String):String = a + b
		}
		implicit object CharStringOptionally extends typelevel.Optionally[Char, String] {
			def none:String = ""
			def some(elem:Char):String = elem.toString
		}
		implicit object StringStringOptionally extends typelevel.Optionally[String, String] {
			def none:String = ""
			def some(elem:String):String = elem
		}

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
	}

	private def StringBase(using Quotes):Parser[Expr[String]] = {
		val DelimiterP:Parser[Unit] = IsString("\"")
		val JCharImmediate:Parser[Char] = CharWhere(c => c >= ' ' && c != '"' && c != '\\', "printable character other than '\"' or '\\'")
		val JCharEscaped:Parser[Char] = (
			(IsString("\\") andThen (
				CharIn("\\/\"") orElse
				IsString("n").map(_ => '\n') orElse
				IsString("r").map(_ => '\r') orElse
				IsString("b").map(_ => '\b') orElse
				IsString("f").map(_ => '\f') orElse
				IsString("t").map(_ => '\t') orElse
				(IsString("u") andThen CharIn(('1' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(4,4).map(x => Integer.parseInt(x, 16).toChar))
			))
		)
		val JCharP:Parser[Char] = JCharEscaped orElse JCharImmediate
		val JCharsI:Parser[Expr[String]] = JCharP.repeat(1, strategy = RepeatStrategy.Possessive).map(Expr.apply _)
		val LiftedV:Parser[Expr[String]] = Lifted[Lift.String, Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(jstringExprToStringExpr _)
		val Content:Parser[Expr[String]] = (LiftedV orElse JCharsI).repeat(strategy = RepeatStrategy.Possessive)
			.map(strs => concatenateStrings(strs))
		(DelimiterP andThenWithCut Content andThen DelimiterP)
	}

	private def JStringP(using Quotes):Parser[Expr[JString]] = {
		StringBase.map(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)})
	}

	private def ArrayP(using Quotes):Parser[Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("[")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("]")

		val LiftedArrayV = Lifted[Lift.Array, Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		)
		val LiftedArrayV2 = LiftedArrayV.map(x => '{ $x.arr })

		val SplicableValue:Parser[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]] = (
			ValueP.map(x => Left(x)) orElse
			(WhitespaceP andThen IsString("..") andThen LiftedArrayV2
				andThen WhitespaceP).map(x => Right(x))
		)
		val LiteralPresplice:Parser[List[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix andThenWithCut (SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive) andThen Suffix)
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => '{ JArray.apply($x)})
	})

	private def ObjectP(using Quotes):Parser[Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("{")
		val Separator:Parser[Unit] = IsString(":")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("}")

		val ObjectV = Lifted[Lift.Object, Expr[JObject]](
			myLiftFunction[JObject, Lift.Object],
			"A for Lift[A, JObject]"
		)
		val ObjectV2 = ObjectV.map(x => '{ $x.obj })

		val KeyValueV = Lifted[Lift.KeyValue, Expr[(java.lang.String, JValue)]](
			myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
			"A for Lift[A, (String, JValue)]"
		)

		val KeyV = {
			val LiftedV:Parser[Expr[String]] = Lifted[Lift.String, Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(jstringExprToStringExpr _)
			val Immediate:Parser[Expr[String]] = StringBase

			WhitespaceP andThen (LiftedV orElse Immediate) andThen WhitespaceP
		}


		val SplicableValue:Parser[Either[Expr[(String, JValue)], Expr[TraversableOnce[(String, JValue)]]]] = (
			(WhitespaceP andThen KeyValueV andThen WhitespaceP)
				.map(x => Left(x)) orElse
			(KeyV andThen Separator andThenWithCut ValueP)
				.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
				.map(x => Left(x)) orElse
			(WhitespaceP andThen IsString("..") andThen ObjectV2 andThen WhitespaceP)
				.map(x => Right(x))
		)
		val LiteralPresplice:Parser[List[Either[Expr[(String, JValue)], Expr[TraversableOnce[(String, JValue)]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix andThenWithCut (SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive) andThen Suffix)
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => '{ JObject.apply($x) })
	})

	private def LiftedP(using Quotes) = Lifted[Lift.Value, Expr[JValue]](
		myLiftFunction[JValue, Lift.Value],
		"Lifted Value"
	)

	private def ValueP(using Quotes):Parser[Expr[JValue]] = {
		(WhitespaceP andThen (
			NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP orElse LiftedP
		) andThen WhitespaceP)
	}

	private def Aggregate(using Quotes) = (ValueP andThen End)

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		Aggregate.parse(sc, args)
	}
}
