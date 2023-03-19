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

	import com.rayrobdod.stringContextParserCombinator.Interpolator._

	private def CharFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] = CharWhere(pf.isDefinedAt).flatMap(pf.apply)

	private val WhitespaceP:Interpolator[Unit] = CharIn("\n\r\t ").repeat(strategy = RepeatStrategy.Possessive).map(_ => ())

	private def NullP(using Quotes):Interpolator[Expr[JNull.type]] = IsString("null").map(_ => '{ _root_.org.json4s.JsonAST.JNull })

	private def BooleanP(using Quotes):Interpolator[Expr[JBool]] = {
		val TrueI = IsString("true").map(_ => '{ _root_.org.json4s.JsonAST.JBool.True })
		val FalseI = IsString("false").map(_ => '{ _root_.org.json4s.JsonAST.JBool.False })
		TrueI orElse FalseI
	}

	private def NumberP(using Quotes):Interpolator[Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper

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
	}

	private def StringBase(using Quotes):Interpolator[Expr[String]] = {
		val DelimiterP:Interpolator[Unit] = IsString("\"")
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
		val JCharsI:Interpolator[Expr[String]] = JCharP.repeat(1, strategy = RepeatStrategy.Possessive).map(Expr.apply _)
		val LiftedV:Interpolator[Expr[String]] = Lifted[Lift.String, Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(jstringExprToStringExpr _)
		val Content:Interpolator[Expr[String]] = (LiftedV orElse JCharsI).repeat(strategy = RepeatStrategy.Possessive)
			.map(strs => concatenateStrings(strs))
		(DelimiterP andThen Content andThen DelimiterP)
	}

	private def JStringP(using Quotes):Interpolator[Expr[JString]] = {
		StringBase.map(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)})
	}

	private def ArrayP(using Quotes):Interpolator[Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Interpolator[Unit] = IsString("[") andThen WhitespaceP
		val Delim:Interpolator[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Interpolator[Unit] = IsString("]")

		val LiftedArrayV = Lifted[Lift.Array, Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		)
		val LiftedArrayV2 = LiftedArrayV.map(x => '{ $x.arr })

		val SplicableValue:Interpolator[Either[Expr[JValue], Expr[List[JValue]]]] = (
			ValueP.map(x => Left(x)) orElse
			(IsString("..") andThen LiftedArrayV2
				andThen WhitespaceP).map(x => Right(x))
		)
		val LiteralPresplice:Interpolator[List[Either[Expr[JValue], Expr[List[JValue]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix andThen (SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive) andThen Suffix)
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => '{ JArray.apply($x)})
	})

	private def ObjectP(using Quotes):Interpolator[Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Interpolator[Unit] = IsString("{") andThen WhitespaceP
		val Separator:Interpolator[Unit] = IsString(":") andThen WhitespaceP
		val Delim:Interpolator[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Interpolator[Unit] = IsString("}")

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
			val LiftedV:Interpolator[Expr[String]] = Lifted[Lift.String, Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(jstringExprToStringExpr _)
			val Immediate:Interpolator[Expr[String]] = StringBase

			(LiftedV orElse Immediate) andThen WhitespaceP
		}


		val SplicableValue:Interpolator[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]] = (
			(KeyValueV andThen WhitespaceP)
				.map(x => Left(x)) orElse
			(KeyV andThen Separator andThen ValueP)
				.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
				.map(x => Left(x)) orElse
			(IsString("..") andThen ObjectV2 andThen WhitespaceP)
				.map(x => Right(x))
		)
		val LiteralPresplice:Interpolator[List[Either[Expr[(String, JValue)], Expr[List[(String, JValue)]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix andThen (SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive) andThen Suffix)
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => '{ JObject.apply($x) })
	})

	private def LiftedP(using Quotes) = Lifted[Lift.Value, Expr[JValue]](
		myLiftFunction[JValue, Lift.Value],
		"Lifted Value"
	)

	private def ValueP(using Quotes):Interpolator[Expr[JValue]] = {
		((
			NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP orElse LiftedP
		) andThen WhitespaceP)
	}

	private def Aggregate(using Quotes) = (WhitespaceP andThen ValueP andThen End)

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		Aggregate.interpolate(sc, args)
	}
}
