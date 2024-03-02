package name.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.Seq
import scala.math.BigDecimal
import scala.reflect.macros.whitebox.Context
import org.json4s._
import name.rayrobdod.stringContextParserCombinator._

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

	private[this] implicit val thisCToExpr:typeclass.ToExprMapping[c.Expr, c.universe.Liftable, c.TypeTag] = typeclass.ToExprMapping.forContext(c)
	private[this] val leafParsers = Parser.contextParsers(c)
	import leafParsers._
	private[this] val myInterpolators = Interpolator.contextInterpolators(c)
	import myInterpolators.lifted
	private[this] val myBiEithered = typeclass.BiEithered.forContext(c)
	import myBiEithered._
	private[this] val myBiRepeateds = typeclass.BiRepeated.forContext(c)
	import myBiRepeateds._

	private[this] def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] = {
		charWhere(pf.isDefinedAt)
			.flatMap(pf.apply)
	}

	private[this] val whitespace:Parser[Unit] = {
		charIn("\n\r\t ")
			.void
			.repeat(strategy = RepeatStrategy.Possessive)
			.hide
	}

	private[this] val jnull:Parser[c.Expr[JNull.type]] = {
		isString("null")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JNull))
			.extractorAtom[c.Expr, c.TypeTag, JNull.type]
	}

	private[this] val booleanValue:Parser[c.Expr[JBool]] = {
		val jTrue = isString("true")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.True))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		val jFalse = isString("false")
			.map(_ => c.universe.reify(_root_.org.json4s.JsonAST.JBool.False))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		jTrue <|> jFalse
	}

	private[this] val jnumber:Parser[c.Expr[JValue with JNumber]] = {
		import Interpolator._
		import scala.Predef.charWrapper

		def RepeatedDigits(min:Int):Interpolator[String] = charIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		implicit object StringStringAndThenTypes extends typeclass.Sequenced[String, String, String] {
			def aggregate(a:String, b:String):String = a + b
		}
		implicit val CharStringOptionallyTypes = typeclass.Optionally[Char, String]("", _.toString)
		implicit val StringStringOptionallyTypes = typeclass.Optionally.whereDefault[String]("")

		val stringRepr: Interpolator[String] = (
			charIn("-").optionally()
			<~> (charIn("0").map(_.toString) <|> (charIn('1' to '9').map(_.toString) <~> RepeatedDigits(0)))
			<~> (charIn(".").map(_.toString) <~> RepeatedDigits(1)).optionally()
			<~> (charIn("eE").map(_.toString) <~> charIn("+-").optionally() <~> RepeatedDigits(1)).optionally()
		)
			.opaque("Number Literal")

		val interpolator = stringRepr
			.map({x =>
				val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
				c.universe.reify(_root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply(xExpr.splice)))
			})

		val extractor = stringRepr
			.map({x =>
				val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
				c.universe.reify(_root_.scala.math.BigDecimal.apply(xExpr.splice))
			})
			.extractorAtom[c.Expr, c.TypeTag, BigDecimal]
			.toExtractor
			.contramap[c.Expr[JValue with JNumber]]({(n: c.Expr[JValue with JNumber]) =>
				c.universe.reify(_root_.name.rayrobdod.stringContextParserCombinatorExample.json.jnumber2bigdecimal(n.splice))
			})

		paired(interpolator, extractor)
	}

	private[this] val stringBase:Parser[c.Expr[String]] = {
		import Interpolator._
		val delimiter:Parser[Unit] = leafParsers.isString("\"")
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
		val jCharsImmediate:Parser[c.Expr[String]] = jChar
			.repeat(1, strategy = RepeatStrategy.Possessive)
			.mapToExpr
			.extractorAtom[c.Expr, c.TypeTag, String]
		val jCharsLifted:Parser[c.Expr[String]] = paired(
			lifted[Lift.String, c.Expr[JString]](
				myLiftFunction[JString, Lift.String],
				"A for Lift[A, JString]"
			).map(x => c.universe.reify(x.splice.values)),
			ofType[String].toExtractor
		)
		val content:Parser[c.Expr[String]] = paired(
			(jCharsLifted <|> jCharsImmediate)
				.toInterpolator
				.repeat[c.Expr[Any], List[c.Expr[String]]](strategy = RepeatStrategy.Possessive)
				.map(strs => concatenateStrings(strs))
			,
			(jCharsImmediate).toExtractor
		)
		(delimiter ~> content <~ delimiter)
	}

	private[this] val jstring:Parser[c.Expr[JString]] = {
		stringBase.imap(
			x => c.universe.reify(_root_.org.json4s.JsonAST.JString.apply(x.splice)),
			x => c.universe.reify(x.splice.values)
		)
	}

	private[this] val jarray:Parser[c.Expr[JArray]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("[") <~> whitespace
		val delimiter:Parser[Unit] = isString(",") <~> whitespace
		val suffix:Parser[Unit] = isString("]")

		val interpolator:Interpolator[c.Expr[JArray]] = {
			val liftedArray = {
				lifted[Lift.Array, c.Expr[JArray]](
					myLiftFunction[JArray, Lift.Array],
					"A for Lift[A, JArray]"
				)
					.map(x => c.Expr[List[JValue]](c.universe.Select(x.tree, c.universe.TermName("arr"))))
					.andThen(whitespace.toInterpolator)
			}

			val splicableValue:Interpolator[Either[c.Expr[JValue], c.Expr[List[JValue]]]] = {
				val value = jvalue.toInterpolator
				val array = (isString("..").toInterpolator ~> liftedArray)
				value <+> array
			}

			val literalPresplice:Interpolator[List[Either[c.Expr[JValue], c.Expr[List[JValue]]]]] = (
				splicableValue.repeat(delimiter = delimiter.toInterpolator, strategy = RepeatStrategy.Possessive)
			)

			literalPresplice
				.map(xs => assembleCollection(xs))
				.map(x => c.universe.reify(JArray.apply(x.splice)))
		}

		val extractor:Extractor[c.Expr[JArray]] = {
			jvalue.toExtractor
				.repeat[c.Expr[List[JValue]]](delimiter = delimiter.toExtractor)
				.contramap((x:c.Expr[JArray]) =>  c.universe.reify(x.splice.arr))
		}

		prefix ~> paired(interpolator, extractor) <~ suffix
	})

	private[this] val jobject:Parser[c.Expr[JObject]] = `lazy`(() => {
		val prefix:Parser[Unit] = isString("{") <~> whitespace
		val separator:Parser[Unit] = isString(":") <~> whitespace
		val delimiter:Parser[Unit] = isString(",") <~> whitespace
		val suffix:Parser[Unit] = isString("}")

		val interpolator:Interpolator[c.Expr[JObject]] = {
			val liftedObject = lifted[Lift.Object, c.Expr[JObject]](
				myLiftFunction[JObject, Lift.Object],
				"A for Lift[A, JObject]"
			).map(x => c.Expr[List[(String, JValue)]](c.universe.Select(x.tree, c.universe.TermName("obj"))))

			val liftedKeyValue = lifted[Lift.KeyValue, c.Expr[(java.lang.String, JValue)]](
				myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
				"A for Lift[A, (String, JValue)]"
			)

			val key = {
				val jCharsLifted:Interpolator[c.Expr[String]] = lifted[Lift.String, c.Expr[JString]](
					myLiftFunction[JString, Lift.String],
					"A for Lift[A, JString]"
				).map(x => c.universe.reify(x.splice.values))
				val immediate:Interpolator[c.Expr[String]] = stringBase.toInterpolator
				(jCharsLifted <|> immediate) <~ whitespace.toInterpolator
			}

			val splicableValue:Interpolator[Either[c.Expr[(String, JValue)], c.Expr[List[(String, JValue)]]]] = {
				val keyValue = (liftedKeyValue <~ whitespace.toInterpolator)
				val keyThenValue = (key <~ separator.toInterpolator <~> jvalue.toInterpolator)
					.map(x => {val (k, v) = x; c.universe.reify(Tuple2.apply(k.splice, v.splice))})
				val mapping = (isString("..").toInterpolator ~> liftedObject <~ whitespace.toInterpolator)
				(keyValue <|> keyThenValue) <+> mapping
			}

			val literalPresplice:Interpolator[List[Either[c.Expr[(String, JValue)], c.Expr[List[(String, JValue)]]]]] = (
					splicableValue.repeat(delimiter = delimiter.toInterpolator, strategy = RepeatStrategy.Possessive)
			)

			literalPresplice
				.map(xs => assembleCollection(xs))
				.map(x => c.universe.reify(JObject.apply(x.splice)))
		}

		val extractor:Extractor[c.Expr[JObject]] = {
			ofType[(String, JValue)].toExtractor
				.repeat[c.Expr[List[(String, JValue)]]](delimiter = delimiter.toExtractor)
				.contramap(x => c.universe.reify(x.splice.obj))
		}

		prefix ~> paired(interpolator, extractor) <~ suffix
	})

	private[this] val jlifted:Parser[c.Expr[JValue]] = {
		paired(
			lifted[Lift.Value, c.Expr[JValue]](
				myLiftFunction[JValue, Lift.Value],
				"Liftable Value"
			),
			ofType[JValue].toExtractor
		)
	}

	private[this] val jvalue:Parser[c.Expr[JValue]] = {
		/*
		 * if this is replaced with `c.universe.reify(value.splice.isInstanceOf[A])`,
		 * scalac complains, despite empirical evidence to the contrary, that `A` is unchecked and the WeakTypeTag is unused
		 */
		def isInstanceOfTree[A](typeTag:c.WeakTypeTag[A])(value:c.Expr[JValue]):c.Expr[Boolean] = {
			c.Expr[Boolean](
				c.universe.TypeApply(
					c.universe.Select(value.tree, c.universe.TermName("isInstanceOf")),
					c.universe.TypeTree(typeTag.tpe) :: Nil
				)
			)
		}

		/*
		 * if this is replaced with `c.universe.reify(value.splice.asInstanceOf[A])`,
		 * scalac complains, despite empirical evidence to the contrary, that `A` is unchecked and the WeakTypeTag is unused
		 */
		def asInstanceOfTree[A](typeTag:c.WeakTypeTag[A])(value:c.Expr[JValue]):c.Expr[A] = {
			c.Expr[A](
				c.universe.TypeApply(
					c.universe.Select(value.tree, c.universe.TermName("asInstanceOf")),
					c.universe.TypeTree(typeTag.tpe) :: Nil
				)
			)(typeTag)
		}

		def widenToJValue[A <: JValue](parser:Parser[c.Expr[A]])(implicit typ:c.WeakTypeTag[A]):Parser[c.Expr[JValue]] = {
			parser.widenWith(
				Predef.identity,
				PartialExprFunction(
					isInstanceOfTree(typ) _,
					asInstanceOfTree(typ) _
				)
			)
		}

		((
			widenToJValue(jnull) <|>
			widenToJValue(booleanValue) <|>
			widenToJValue(jnumber) <|>
			widenToJValue(jstring) <|>
			widenToJValue(jarray) <|>
			widenToJValue(jobject) <|>
			jlifted
		) <~ whitespace)
	}

	private[this] val onlyJvalue = (whitespace <~> jvalue <~> end)

	private val className = "name.rayrobdod.stringContextParserCombinatorExample.json.package.JsonStringContext"
	def stringContext_json(args:c.Expr[Any]*):c.Expr[JValue] = {
		onlyJvalue.interpolate(c)(className)(args.toList)
	}

	def stringContext_json_unapply(value:c.Expr[JValue]):c.Expr[Any] = {
		onlyJvalue.extractor[JValue](c)(className)(value)
	}
}
