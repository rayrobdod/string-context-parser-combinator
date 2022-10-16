package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
import scala.reflect.macros.blackbox.Context
import org.json4s.{JValue, JNull, JBool, JNumber, JString, JArray, JObject}
import com.rayrobdod.stringContextParserCombinator._

final class MacroImpl(val c:Context {type PrefixType = JsonStringContext}) {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	private[this] def concatenateStrings(strings:Seq[c.Expr[String]]):c.Expr[String] = {
		strings match {
			case Seq() => c.Expr[String](c.universe.Literal(c.universe.Constant("")))
			case Seq(x) => x
			case _ => {
				val accumulatorName = c.universe.TermName("accumulator$")
				val accumulatorType = c.universe.typeTag[scala.collection.mutable.StringBuilder]
				val accumulatorTypeTree = c.universe.TypeTree(accumulatorType.tpe)
				val accumulatorExpr = c.Expr(c.universe.Ident(accumulatorName))(accumulatorType)
				val stats = scala.collection.mutable.Buffer[c.universe.Tree](
					c.universe.ValDef(
						c.universe.NoMods,
						accumulatorName,
						accumulatorTypeTree,
						c.universe.Apply(
							c.universe.Select(
								c.universe.New(accumulatorTypeTree),
								c.universe.termNames.CONSTRUCTOR
							),
							List()
						)
					)
				)
				strings.foreach(x => stats += c.universe.reify(accumulatorExpr.splice.append(x.splice)).tree)

				c.Expr[String](
					c.universe.Block(
						stats.toList,
						c.universe.reify(accumulatorExpr.splice.toString).tree
					)
				)
			}
		}
	}

	private[this] def assembleCollection[A](parts:List[Either[c.Expr[A], c.Expr[TraversableOnce[A]]]])(implicit builderType: c.universe.TypeTag[scala.collection.mutable.Builder[A, List[A]]]):c.Expr[List[A]] = {
		val builderName = c.freshName(c.universe.TermName("builder"))
		val builderTypeTree = c.universe.TypeTree(builderType.tpe)
		val builderExpr = c.Expr(c.universe.Ident(builderName))(builderType)

		val createBuilder = c.universe.ValDef(
			c.universe.NoMods,
			builderName,
			builderTypeTree,
			c.universe.reify(List.newBuilder).tree
		)
		val insertBuilder = parts.map(part => part match {
			case Left(single) => c.universe.reify(builderExpr.splice.+=(single.splice)).tree
			case Right(group) => c.universe.reify(builderExpr.splice.++=(group.splice)).tree
		})

		c.Expr[List[A]](
			c.universe.Block(
				createBuilder :: insertBuilder,
				c.universe.reify(builderExpr.splice.result()).tree
			)
		)
	}

	private[this] def myLiftFunction[Z, Lifter[A] <: Lift[A, Z]]:LiftFunction[c.type, Lifter, c.Expr[Z]] = {
		new LiftFunction[c.type, Lifter, c.Expr[Z]] {
			def apply[A](lifter:c.Expr[Lifter[A]], a:c.Expr[A]):c.Expr[Z] = {
				c.Expr(
					c.universe.Apply(
						c.universe.Select(
							lifter.tree,
							c.universe.TermName("apply")
						),
						List(a.tree)
					)
				)
			}
		}
	}

	private[this] val LeafParsers = Parsers(c)
	import LeafParsers._

	private[this] val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat(strategy = RepeatStrategy.Possessive).map(_ => ()).opaque("Whitespace")

	private[this] val NullP:Parser[c.Expr[JNull.type]] = IsString("null").map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JNull))

	private[this] val BooleanP:Parser[c.Expr[JBool]] = {
		val TrueI = IsString("true").map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.True))
		val FalseI = IsString("false").map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.False))
		TrueI orElse FalseI
	}

	private[this] val NumberP:Parser[c.Expr[JValue with JNumber]] = {
		import scala.Predef.charWrapper

		def RepeatedDigits(min:Int):Parser[String] = CharIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		implicit object StringStringAndThenTypes extends typelevel.Sequenced[String, String, String] {
			def aggregate(a:String, b:String):String = a + b
		}
		implicit object CharStringOptionallyTypes extends typelevel.Optionally[Char, String] {
			def none:String = ""
			def some(elem:Char):String = elem.toString
		}
		implicit object StringStringOptionallyTypes extends typelevel.Optionally[String, String] {
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
				val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
				c.universe.reify(_root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply(xExpr.splice)))
			})
			.opaque("Number Literal")
	}

	private[this] val StringBase:Parser[c.Expr[String]] = {
		val DelimiterP:Parser[Unit] = IsString("\"")
		val JCharImmediate:Parser[Char] = CharWhere(c => c >= ' ' && c != '"' && c != '\\').opaque("printable character other than '\"' or '\\'")
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
		val JCharsI:Parser[c.Expr[String]] = JCharP.repeat(1, strategy = RepeatStrategy.Possessive)
			.map(x => c.Expr(c.universe.Literal(c.universe.Constant(x))))
		val LiftedV:Parser[c.Expr[String]] = Lifted[Lift.String, c.Expr[JString]](
			myLiftFunction[JString, Lift.String],
			"A for Lift[A, JString]"
		).map(x => c.universe.reify(x.splice.values))
		val Content:Parser[c.Expr[String]] = (LiftedV orElse JCharsI).repeat(strategy = RepeatStrategy.Possessive)
			.map(strs => concatenateStrings(strs))
		(DelimiterP andThenWithCut Content andThen DelimiterP)
	}

	private[this] val JStringP:Parser[c.Expr[JString]] = {
		StringBase.map(x => c.universe.reify(_root_.org.json4s.JsonAST.JString.apply(x.splice)))
	}

	private[this] val ArrayP:Parser[c.Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("[")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("]")

		val LiftedArrayV = Lifted[Lift.Array, c.Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		)
			.map(x => c.Expr[Vector[JValue]](c.universe.Select(x.tree, c.universe.TermName("arr"))))

		val SplicableValue:Parser[Either[c.Expr[JValue], c.Expr[TraversableOnce[JValue]]]] = {
			val value = ValueP
			val array = (WhitespaceP andThen IsString("..") andThenWithCut LiftedArrayV
				andThen WhitespaceP)
			value.orElse(array)(typelevel.Eithered.discriminatedUnion)
		}
		val LiteralPresplice:Parser[List[Either[c.Expr[JValue], c.Expr[TraversableOnce[JValue]]]]] = (
			Prefix
				andThenWithCut SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive)
				andThen Suffix
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => c.universe.reify(JArray.apply(x.splice)))
	})

	private[this] val ObjectP:Parser[c.Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("{")
		val Separator:Parser[Unit] = IsString(":")
		val Delim:Parser[Unit] = IsString(",")
		val Suffix:Parser[Unit] = IsString("}")

		val ObjectV = Lifted[Lift.Object, c.Expr[JObject]](
			myLiftFunction[JObject, Lift.Object],
			"A for Lift[A, JObject]"
		).map(x => c.Expr[Map[String, JValue]](c.universe.Select(x.tree, c.universe.TermName("obj"))))

		val KeyValueV = Lifted[Lift.KeyValue, c.Expr[(java.lang.String, JValue)]](
			myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
			"A for Lift[A, (String, JValue)]"
		)

		val KeyV = {
			val LiftedV:Parser[c.Expr[String]] = Lifted[Lift.String, c.Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(x => c.universe.reify(x.splice.values))
			val Immediate:Parser[c.Expr[String]] = StringBase
			WhitespaceP andThen (LiftedV orElse Immediate) andThen WhitespaceP
		}

		val SplicableValue:Parser[Either[c.Expr[(String, JValue)], c.Expr[TraversableOnce[(String, JValue)]]]] = {
			val keyValue = (WhitespaceP andThen KeyValueV andThen WhitespaceP)
			val keyThenValue = (KeyV andThen Separator andThenWithCut ValueP)
				.map(x => {val (k, v) = x; c.universe.reify(Tuple2.apply(k.splice, v.splice))})
			val mapping = (WhitespaceP andThen IsString("..") andThenWithCut ObjectV
				andThen WhitespaceP)
			keyValue.orElse(keyThenValue).orElse(mapping)(typelevel.Eithered.discriminatedUnion)
		}
		val LiteralPresplice:Parser[List[Either[c.Expr[(String, JValue)], c.Expr[TraversableOnce[(String, JValue)]]]]] = (
			Prefix
				andThenWithCut SplicableValue.repeat(delimiter = Delim, strategy = RepeatStrategy.Possessive)
				andThen Suffix
		)

		LiteralPresplice
			.map(xs => assembleCollection(xs))
			.map(x => c.universe.reify(JObject.apply(x.splice)))
	})

	private[this] val LiftedP:Parser[c.Expr[JValue]] = {
		Lifted[Lift.Value, c.Expr[JValue]](
			myLiftFunction[JValue, Lift.Value],
			"Liftable Value"
		)
	}

	private[this] val ValueP:Parser[c.Expr[JValue]] = {
		(WhitespaceP andThen (
			NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP orElse LiftedP
		) andThen WhitespaceP)
	}

	private[this] val Aggregate = (ValueP andThen End)

	def stringContext_json(args:c.Expr[Any]*):c.Expr[JValue] = {
		val className = "com.rayrobdod.stringContextParserCombinatorExample.json.package.JsonStringContext"
		Aggregate.parse(c)(className)(args.toList)
	}
}
