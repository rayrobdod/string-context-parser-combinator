package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
import scala.quoted.{Expr, Quotes, Type}
import org.json4s.JsonAST._
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

	private trait CollectionAssembly[A, CC] {
		type Builder
		def builderType(using Quotes):Type[Builder]
		def newBuilder(using Quotes):Expr[Builder]
		def insertOne(builder:Expr[Builder], item:Expr[A])(using Quotes):Expr[Builder]
		def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[A]])(using Quotes):Expr[Builder]
		def result(builder:Expr[Builder])(using Quotes):Expr[CC]
	}

	private object VectorCollectionAssembly extends CollectionAssembly[JValue, List[JValue]] {
		type Builder = scala.collection.mutable.Builder[JValue, List[JValue]]
		override def builderType(using Quotes):Type[Builder] = implicitly[Type[Builder]]
		override def newBuilder(using Quotes):Expr[Builder] = '{ List.newBuilder[JValue] }
		override def insertOne(builder:Expr[Builder], item:Expr[JValue])(using Quotes):Expr[Builder] = '{ $builder.addOne($item) }
		override def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[JValue]])(using Quotes):Expr[Builder] = '{ $builder.addAll($items) }
		override def result(builder:Expr[Builder])(using Quotes):Expr[List[JValue]] = '{ $builder.result() }
	}

	private object MapCollectionAssembly extends CollectionAssembly[(String, JValue), List[(String, JValue)]] {
		type Builder = scala.collection.mutable.Builder[(String, JValue), List[(String, JValue)]]
		override def builderType(using Quotes):Type[Builder] = implicitly[Type[Builder]]
		override def newBuilder(using Quotes):Expr[Builder] = '{ List.newBuilder[(String, JValue)] }
		override def insertOne(builder:Expr[Builder], item:Expr[(String, JValue)])(using Quotes):Expr[Builder] = '{ $builder.addOne($item) }
		override def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[(String, JValue)]])(using Quotes):Expr[Builder] = '{ $builder.addAll($items) }
		override def result(builder:Expr[Builder])(using Quotes):Expr[List[(String, JValue)]] = '{ $builder.result() }
	}

	private def assembleCollection[A : Type, CC : Type]
			(assembly:CollectionAssembly[A, CC])
			(parts:List[Either[Expr[A], Expr[TraversableOnce[A]]]])
			(using Quotes)
	:Expr[CC] = {
		given scala.quoted.Type[assembly.Builder] = assembly.builderType
		assembly.result(
			parts.foldLeft(assembly.newBuilder)({(builder, part) =>
				part match {
					case Left(single) => assembly.insertOne(builder, single)
					case Right(group) => assembly.insertMany(builder, group)
				}
			})
		)
	}

	import scala.language.higherKinds
	private def myLiftFunction[Z : Type, Lifter[A] <: Lift[A, Z] : Type]:LiftFunction[Lifter, Expr[Z]] = {
		new LiftFunction[Lifter, Expr[Z]] {
			def apply[A : Type](lifter:Expr[Lifter[A]], a:Expr[A])(using Quotes):Expr[Z] = {
				'{ $lifter.apply($a) }
			}
		}
	}

	/**
	 * A micro-optimization; basically just removes a call to an identity function if Lifted created one
	 */
	private def unwrapIdentityLift[A <: JValue : Type](using Quotes)(in:Expr[A]):Expr[A] = in match {
		case '{ com.rayrobdod.stringContextParserCombinatorExample.json.Lift.jvalue[A].apply(${param}) } => param
		case _ => in
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
	private val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat().map(_ => ())

	private def NullP(using Quotes):Parser[Expr[JNull.type]] = IsString("null").map(_ => '{ _root_.org.json4s.JsonAST.JNull })

	private def BooleanP(using Quotes):Parser[Expr[JBool]] = {
		val TrueI = IsString("true").map(_ => '{ _root_.org.json4s.JsonAST.JBool.True })
		val FalseI = IsString("false").map(_ => '{ _root_.org.json4s.JsonAST.JBool.False })
		val LiftedV = Lifted[Lift.Boolean, Expr[JBool]](
			myLiftFunction[JBool, Lift.Boolean],
			"A for Lift[A, JBool]"
		).map(unwrapIdentityLift _)
		LiftedV orElse TrueI orElse FalseI
	}

	private def NumberP(using Quotes):Parser[Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper

		val NumberI:Parser[Expr[JValue with JNumber]] = {
			def RepeatedDigits(min:Int):Parser[String] = CharIn('0' to '9').repeat(min)

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
				CharIn("-").optionally
				andThen (CharIn("0").map(_.toString) orElse (CharIn('1' to '9').map(_.toString) andThen RepeatedDigits(0)))
				andThen (CharIn(".").map(_.toString) andThen RepeatedDigits(1)).optionally
				andThen (CharIn("eE").map(_.toString) andThen CharIn("+-").optionally andThen RepeatedDigits(1)).optionally
			).map({x =>
				'{ _root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply( ${Expr[String](x)} )) }
			})
		}.opaque("Number Literal")
		val LiftedV = Lifted[Lift.Number, Expr[JValue with JNumber]](
			myLiftFunction[JValue with JNumber, Lift.Number],
			"A for Lift[A, JNumber]"
		).map(unwrapIdentityLift _)
		LiftedV orElse NumberI
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
		val JCharsI:Parser[Expr[String]] = JCharP.repeat(1).map(Expr.apply _)
		val LiftedV:Parser[Expr[String]] = Lifted[Lift.String, Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(jstringExprToStringExpr _)
		val Content:Parser[Expr[String]] = (LiftedV orElse JCharsI).repeat()
			.map(strs => concatenateStrings(strs))
		(DelimiterP andThenWithCut Content andThen DelimiterP)
	}

	private def StringP(using Quotes):Parser[Expr[String]] = {
		val LiftedV:Parser[Expr[String]] = Lifted[Lift.String, Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(jstringExprToStringExpr _)
		val Immediate:Parser[Expr[String]] = StringBase
		LiftedV orElse Immediate
	}

	private def JStringP(using Quotes):Parser[Expr[JString]] = {
		val LiftedV:Parser[Expr[JString]] = Lifted[Lift.String, Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(unwrapIdentityLift _)
		val Immediate:Parser[Expr[JString]] = StringBase.map(x => '{ _root_.org.json4s.JsonAST.JString.apply($x)})
		LiftedV orElse Immediate
	}

	private def ArrayP(using Quotes):Parser[Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("[")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("]")

		val LiftedArrayV = Lifted[Lift.Array, Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		).map(unwrapIdentityLift _)
		val LiftedArrayV2 = LiftedArrayV.map(x => '{ $x.arr })

		val SplicableValue:Parser[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]] = (
			ValueP.map(x => Left(x)) orElse
			(WhitespaceP andThen IsString("..") andThen LiftedArrayV2
				andThen WhitespaceP).map(x => Right(x))
		)
		val LiteralPresplice:Parser[List[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]]] = (
			// somehow manages to widen its type to `List[Matchable]` if the order of operations is different
			Prefix andThenWithCut (SplicableValue.repeat(delimiter = Delim) andThen Suffix)
		)
		val Literal:Parser[Expr[JArray]] = (
			LiteralPresplice
				.map(xs => assembleCollection(VectorCollectionAssembly)(xs))
				.map(x => '{ JArray.apply($x)})
		)

		LiftedArrayV orElse Literal
	})

	private def ObjectP(using Quotes):Parser[Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("{")
		val Separator:Parser[Unit] = IsString(":")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("}")

		val ObjectV = Lifted[Lift.Object, Expr[JObject]](
			myLiftFunction[JObject, Lift.Object],
			"A for Lift[A, JObject]"
		).map(unwrapIdentityLift _)
		val ObjectV2 = ObjectV.map(x => '{ $x.obj })

		val KeyValueV = Lifted[Lift.KeyValue, Expr[(java.lang.String, JValue)]](
			myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
			"A for Lift[A, (String, JValue)]"
		)

		val KeyV = WhitespaceP andThen StringP andThen WhitespaceP


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
			Prefix andThenWithCut (SplicableValue.repeat(delimiter = Delim) andThen Suffix)
		)
		val Literal:Parser[Expr[JObject]] = (
			LiteralPresplice
				.map(xs => assembleCollection(MapCollectionAssembly)(xs))
				.map(x => '{ JObject.apply($x) })
		)

		ObjectV orElse Literal
	})

	private def ValueP(using Quotes):Parser[Expr[JValue]] = {
		(WhitespaceP andThen (
			NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP
		) andThen WhitespaceP)
	}

	private def Aggregate(using Quotes) = (ValueP andThen End())

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		macroimpl(Aggregate)(sc, args)
	}
}
