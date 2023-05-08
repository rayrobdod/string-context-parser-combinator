package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.Seq
import scala.reflect.macros.whitebox.Context
import org.json4s.{JValue, JNull, JBool, JNumber, JString, JArray, JObject}
import com.rayrobdod.stringContextParserCombinator._

final class MacroImpl(val c:Context {type PrefixType = JsonStringContext}) {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	private[this] def concatenateStrings(strings:List[c.Expr[String]]):c.Expr[String] = {
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

	private[this] def assembleCollection[A](parts:List[Either[c.Expr[A], c.Expr[List[A]]]])(implicit builderType: c.universe.TypeTag[scala.collection.mutable.Builder[A, List[A]]]):c.Expr[List[A]] = {
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

	private[this] implicit val thisCToExpr = typeclass.ToExprMapping.forContext(c)
	private[this] val leafParsers = Parser.contextParsers(c)
	import leafParsers._
	private[this] val myInterpolators = Interpolator.contextInterpolators(c)
	import myInterpolators.Lifted
	private[this] val myBiEithered = typeclass.BiEithered.forContext(c)
	import myBiEithered._
	private[this] val myBiRepeateds = typeclass.BiRepeated.forContext(c)
	import myBiRepeateds._

	private[this] def CharFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] = {
		CharWhere(pf.isDefinedAt)
			.flatMap(pf.apply)
	}

	private[this] val WhitespaceP:Parser[Unit] = {
		CharIn("\n\r\t ")
			.imap((_:Char) => (), (_:Unit) => ' ')
			.repeat(strategy = RepeatStrategy.Possessive)
			.opaque("Whitespace")
	}

	private[this] val NullP:Parser[c.Expr[JNull.type]] = {
		IsString("null")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JNull))
			.extractorAtom[c.Expr, c.TypeTag, JNull.type]
	}

	private[this] val BooleanP:Parser[c.Expr[JBool]] = {
		val TrueI = IsString("true")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.True))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		val FalseI = IsString("false")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.False))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		TrueI orElse FalseI
	}

	private[this] val NumberP:Parser[c.Expr[JValue with JNumber]] = {
		import Interpolator._
		import scala.Predef.charWrapper

		def RepeatedDigits(min:Int):Interpolator[String] = CharIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		implicit object StringStringAndThenTypes extends typeclass.Sequenced[String, String, String] {
			def aggregate(a:String, b:String):String = a + b
		}
		implicit val CharStringOptionallyTypes = typeclass.Optionally[Char, String]("", _.toString)
		implicit val StringStringOptionallyTypes = typeclass.Optionally.whereDefault[String]("")

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
			.extractorAtom[c.Expr, c.TypeTag, JValue with JNumber]
	}

	private[this] val StringBase:Parser[c.Expr[String]] = {
		import Interpolator._
		val DelimiterP:Parser[Unit] = leafParsers.IsString("\"")
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
		val JCharsI:Parser[c.Expr[String]] = JCharP
			.repeat(1, strategy = RepeatStrategy.Possessive)
			.mapToExpr
			.extractorAtom[c.Expr, c.TypeTag, String]
		val LiftedV:Parser[c.Expr[String]] = Paired(
			Lifted[Lift.String, c.Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(x => c.universe.reify(x.splice.values)),
			OfType[String].toExtractor
		)
		val Content:Parser[c.Expr[String]] = Paired(
			(LiftedV orElse JCharsI)
				.toInterpolator
				.repeat[c.Expr[Any], List[c.Expr[String]]](strategy = RepeatStrategy.Possessive)
				.map(strs => concatenateStrings(strs))
			,
			(JCharsI).toExtractor
		)
		(DelimiterP andThen Content andThen DelimiterP)
	}

	private[this] val JStringP:Parser[c.Expr[JString]] = {
		StringBase.imap(
			x => c.universe.reify(_root_.org.json4s.JsonAST.JString.apply(x.splice)),
			x => c.universe.reify(x.splice.values)
		)
	}

	private[this] val ArrayP:Parser[c.Expr[JArray]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("[") andThen WhitespaceP
		val Delim:Parser[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Parser[Unit] = IsString("]")

		val LiftedArrayV = Lifted[Lift.Array, c.Expr[JArray]](
			myLiftFunction[JArray, Lift.Array],
			"A for Lift[A, JArray]"
		)
			.map(x => c.Expr[List[JValue]](c.universe.Select(x.tree, c.universe.TermName("arr"))))

		val SplicableValue:Interpolator[Either[c.Expr[JValue], c.Expr[List[JValue]]]] = {
			val value = ValueP.toInterpolator
			val array = (IsString("..").toInterpolator andThen LiftedArrayV andThen WhitespaceP.toInterpolator)
			value.orElse(array)(typeclass.Eithered.discriminatedUnion)
		}
		val LiteralPresplice:Interpolator[List[Either[c.Expr[JValue], c.Expr[List[JValue]]]]] = (
			Prefix.toInterpolator
				andThen SplicableValue.repeat(delimiter = Delim.toInterpolator, strategy = RepeatStrategy.Possessive)
				andThen Suffix.toInterpolator
		)

		Paired(
			LiteralPresplice
				.map(xs => assembleCollection(xs))
				.map(x => c.universe.reify(JArray.apply(x.splice)))
			,
			(Prefix.toExtractor
					andThen[c.Expr[List[JValue]], c.Expr[List[JValue]]] (ValueP.toExtractor.repeat(delimiter = Delim.toExtractor))
					andThen[Unit, c.Expr[List[JValue]]] Suffix.toExtractor
			)
				.contramap((x:c.Expr[JArray]) =>  c.universe.reify(x.splice.arr))
		)
	})

	private[this] val ObjectP:Parser[c.Expr[JObject]] = DelayedConstruction(() => {
		val Prefix:Parser[Unit] = IsString("{") andThen WhitespaceP
		val Separator:Parser[Unit] = IsString(":") andThen WhitespaceP
		val Delim:Parser[Unit] = IsString(",") andThen WhitespaceP
		val Suffix:Parser[Unit] = IsString("}")

		val ObjectV = Lifted[Lift.Object, c.Expr[JObject]](
			myLiftFunction[JObject, Lift.Object],
			"A for Lift[A, JObject]"
		).map(x => c.Expr[List[(String, JValue)]](c.universe.Select(x.tree, c.universe.TermName("obj"))))

		val KeyValueV = Lifted[Lift.KeyValue, c.Expr[(java.lang.String, JValue)]](
			myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
			"A for Lift[A, (String, JValue)]"
		)

		val KeyV = {
			val LiftedV:Interpolator[c.Expr[String]] = Lifted[Lift.String, c.Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(x => c.universe.reify(x.splice.values))
			val Immediate:Interpolator[c.Expr[String]] = StringBase.toInterpolator
			(LiftedV orElse Immediate) andThen WhitespaceP.toInterpolator
		}

		val SplicableValue:Interpolator[Either[c.Expr[(String, JValue)], c.Expr[List[(String, JValue)]]]] = {
			val keyValue = (KeyValueV andThen WhitespaceP.toInterpolator)
			val keyThenValue = (KeyV andThen Separator.toInterpolator andThen ValueP.toInterpolator)
				.map(x => {val (k, v) = x; c.universe.reify(Tuple2.apply(k.splice, v.splice))})
			val mapping = (IsString("..").toInterpolator andThen ObjectV andThen WhitespaceP.toInterpolator)
			keyValue.orElse(keyThenValue).orElse(mapping)(typeclass.Eithered.discriminatedUnion)
		}
		val LiteralPresplice:Interpolator[List[Either[c.Expr[(String, JValue)], c.Expr[List[(String, JValue)]]]]] = (
			Prefix.toInterpolator
				andThen SplicableValue.repeat(delimiter = Delim.toInterpolator, strategy = RepeatStrategy.Possessive)
				andThen Suffix.toInterpolator
		)

		Paired(
			LiteralPresplice
				.map(xs => assembleCollection(xs))
				.map(x => c.universe.reify(JObject.apply(x.splice)))
			,
			(Prefix.toExtractor
				andThen[c.Expr[List[(String, JValue)]], c.Expr[List[(String, JValue)]]] (OfType[(String, JValue)].toExtractor.repeat(delimiter = Delim.toExtractor))
				andThen[Unit, c.Expr[List[(String, JValue)]]] Suffix.toExtractor
			)
				.contramap(x => c.universe.reify(x.splice.obj))
		)
	})

	private[this] val LiftedP:Parser[c.Expr[JValue]] = {
		Paired(
			Lifted[Lift.Value, c.Expr[JValue]](
				myLiftFunction[JValue, Lift.Value],
				"Liftable Value"
			),
			OfType[JValue].toExtractor
		)
	}

	private[this] val ValueP:Parser[c.Expr[JValue]] = {
		def widenToJValue[A <: JValue](parser:Parser[c.Expr[A]])(implicit typ:c.WeakTypeTag[A]):Parser[c.Expr[JValue]] = {
			parser.widenWith(
				Predef.identity,
				PartialExprFunction(
					value => c.universe.reify(value.splice.isInstanceOf[A]),
					value => c.universe.reify(value.splice.asInstanceOf[A])
				)
			)
		}

		((
			widenToJValue(NullP) orElse
			widenToJValue(BooleanP) orElse
			widenToJValue(NumberP) orElse
			widenToJValue(JStringP) orElse
			widenToJValue(ArrayP) orElse
			widenToJValue(ObjectP) orElse
			LiftedP
		) andThen WhitespaceP)
	}

	private[this] val Aggregate = (WhitespaceP andThen ValueP andThen End)

	private val className = "com.rayrobdod.stringContextParserCombinatorExample.json.package.JsonStringContext"
	def stringContext_json(args:c.Expr[Any]*):c.Expr[JValue] = {
		Aggregate.interpolate(c)(className)(args.toList)
	}

	def stringContext_json_unapply(value:c.Expr[JValue]):c.Expr[Any] = {
		Aggregate.extractor[JValue](c)(className)(value)
	}
}
