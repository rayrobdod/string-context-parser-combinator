package name.rayrobdod.stringContextParserCombinatorExample.json

import scala.math.BigDecimal
import scala.reflect.macros.whitebox.Context
import org.json4s._
import name.rayrobdod.stringContextParserCombinator._

final class MacroImpl(val c:Context {type PrefixType = JsonStringContext}) {
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

	private[this] implicit val thisCToExpr:typeclass.ToExprMapping[c.type, c.Expr, c.universe.Liftable, c.TypeTag] = typeclass.ToExprMapping.forContext(c)
	private[this] val leafParsers = Parser.contextParsers(c)
	import leafParsers._
	private[this] val myInterpolators = Interpolator.contextInterpolators(c)
	import myInterpolators.lifted

	// 2.13 can find these without the local variable. 2.12 cannot
	private[this] implicit def mySymmetricBiEithered[A]: typeclass.BiEithered[c.type, c.Expr, A, A, A] = typeclass.BiEithered.contextSymmetric[c.type, A]
	private[this] implicit def myUnitBiRepeated: typeclass.BiRepeated[c.type, c.Expr, Unit, Unit] = typeclass.BiRepeated.contextUnit[c.type]
	private[this] implicit def myToListBiRepeated[A](implicit typA: c.TypeTag[A]): typeclass.BiRepeated[c.type, c.Expr, c.Expr[A], c.Expr[List[A]]] = typeclass.BiRepeated.contextToExprList[c.type, A]

	private[this] def charFlatCollect[A](pf: PartialFunction[Char, Interpolator[A]]):Interpolator[A] = {
		charWhere(pf.isDefinedAt)
			.flatMap((x, _) => pf.apply(x))
	}

	private[this] val whitespace:Parser[Unit] = {
		charIn("\n\r\t ")
			.void
			.repeat(strategy = RepeatStrategy.Possessive)
			.hide
	}

	private[this] val jnull:Parser[c.Expr[JNull.type]] = {
		isString("null")
			.map((_, _: c.type) => c.universe.reify(_root_.org.json4s.JsonAST.JNull))
			.extractorAtom[c.Expr, c.TypeTag, JNull.type]
	}

	private[this] val booleanValue:Parser[c.Expr[JBool]] = {
		val jTrue = isString("true")
			.map((_,_) => c.universe.reify(_root_.org.json4s.JsonAST.JBool.True))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		val jFalse = isString("false")
			.map((_,_) => c.universe.reify(_root_.org.json4s.JsonAST.JBool.False))
			.extractorAtom[c.Expr, c.TypeTag, JBool]
		jTrue <|> jFalse
	}

	private[this] val jnumber:Parser[c.Expr[JValue with JNumber]] = {
		import myInterpolators._
		import scala.Predef.charWrapper

		def RepeatedDigits(min:Int):Interpolator[String] = charIn('0' to '9').repeat(min, strategy = RepeatStrategy.Possessive)

		/* Concatenate every capture in the following parser and combine into one long string */
		implicit object StringStringAndThenTypes extends typeclass.Sequenced[c.type, String, String, String] {
			def aggregate(a:String, b:String)(implicit ctx:c.type):String = a + b
		}
		implicit val CharStringOptionallyTypes = typeclass.Optionally[c.type, Char, String](_ => "", (n,_) => n.toString)
		implicit val StringStringOptionallyTypes = typeclass.Optionally.whereDefault[c.type, String](_ => "")

		val toString: (Char, MacroImpl.this.c.type) => String = (c: Char, _: MacroImpl.this.c.type) => c.toString

		val stringRepr: Interpolator[String] = (
			charIn("-").optionally()
			<~> (charIn("0").map(toString) <|> (charIn('1' to '9').map(toString) <~> RepeatedDigits(0)))
			<~> (charIn(".").map(toString) <~> RepeatedDigits(1)).optionally()
			<~> (charIn("eE").map(toString) <~> charIn("+-").optionally() <~> RepeatedDigits(1)).optionally()
		)
			.opaque("Number Literal")

		val interpolator = stringRepr
			.map({(x, _: c.type) =>
				val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
				c.universe.reify(_root_.org.json4s.JsonAST.JDecimal(_root_.scala.math.BigDecimal.apply(xExpr.splice)))
			})

		val extractor = stringRepr
			.map({(x, _: c.type) =>
				val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
				c.universe.reify(_root_.scala.math.BigDecimal.apply(xExpr.splice))
			})
			.extractorAtom[c.Expr, c.TypeTag, BigDecimal]
			.toExtractor
			.contramap[c.Expr[JValue with JNumber]]({(n: c.Expr[JValue with JNumber], _: c.type) =>
				c.universe.reify(_root_.name.rayrobdod.stringContextParserCombinatorExample.json.jnumber2bigdecimal(n.splice))
			})

		paired(interpolator, extractor)
	}

	private[this] val stringBase:Parser[c.Expr[String]] = {
		import myInterpolators._
		val delimiter:Parser[Unit] = leafParsers.isString("\"")
		val jCharImmediate:Interpolator[Char] = charWhere(c => c >= ' ' && c != '"' && c != '\\').opaque("printable character other than '\"' or '\\'")
		val jCharEscaped:Interpolator[Char] = (
			(isString("\\") ~> charFlatCollect({
				case '\\' => pass.map((_, _:c.type) => '\\')
				case '/' => pass.map((_, _:c.type) => '/')
				case '"' => pass.map((_, _:c.type) => '"')
				case 'n' => pass.map((_, _:c.type) => '\n')
				case 'r' => pass.map((_, _:c.type) => '\r')
				case 'b' => pass.map((_, _:c.type) => '\b')
				case 'f' => pass.map((_, _:c.type) => '\f')
				case 't' => pass.map((_, _:c.type) => '\t')
				case 'u' => charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(4,4).map((x, _:c.type) => Integer.parseInt(x, 16).toChar)
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
			).map((x, _:c.type) => c.universe.reify(x.splice.values)),
			Extractor.contextExtractors(c).ofType[String]
		)
		val content:Parser[c.Expr[String]] = paired(
			(jCharsLifted <|> jCharsImmediate)
				.toInterpolator
				.repeat(strategy = RepeatStrategy.Possessive)(typeclass.Repeated.contextConcatenateString)
			,
			(jCharsImmediate).toExtractor
		)
		(delimiter ~> content <~ delimiter)
	}

	private[this] val jstring:Parser[c.Expr[JString]] = {
		stringBase.imap(
			(x, ctx) => ctx.universe.reify(_root_.org.json4s.JsonAST.JString.apply(x.splice)),
			(x, ctx) => ctx.universe.reify(x.splice.values)
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
					.map((x: c.Expr[JArray], ctx: c.type) => ctx.Expr[List[JValue]](ctx.universe.Select(x.tree, ctx.universe.TermName("arr"))))
					.andThen(whitespace.toInterpolator)
			}

			val splicableValues: Interpolator[typeclass.Repeated.SplicePiece[c.Expr, JValue]] = {
				implicit val eitherSplicePiece: typeclass.Eithered[c.type, c.Expr[JValue], c.Expr[List[JValue]], typeclass.Repeated.SplicePiece[c.Expr,JValue]] = typeclass.Eithered.forContext(c).splicePiece

				val value = jvalue.toInterpolator
				val array = (isString("..").toInterpolator ~> liftedArray)
				value <|> array
			}

			splicableValues
				.repeat(
					delimiter = delimiter.toInterpolator,
					strategy = RepeatStrategy.Possessive,
				)(using typeclass.Repeated.contextFromSplicesToExprList[c.type, JValue])
				.map((x: c.Expr[List[JValue]], ctx:c.type) => ctx.universe.reify(JArray.apply(x.splice)))
		}

		val extractor:Extractor[c.Expr[JArray]] = {
			jvalue.toExtractor
				.repeat[c.Expr[List[JValue]]](delimiter = delimiter.toExtractor)
				.contramap((x:c.Expr[JArray], _:c.type) => c.universe.reify(x.splice.arr))
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
			).map((x, _:c.type) => c.Expr[List[(String, JValue)]](c.universe.Select(x.tree, c.universe.TermName("obj"))))

			val liftedKeyValue = lifted[Lift.KeyValue, c.Expr[(java.lang.String, JValue)]](
				myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
				"A for Lift[A, (String, JValue)]"
			)

			val key = {
				val jCharsLifted:Interpolator[c.Expr[String]] = lifted[Lift.String, c.Expr[JString]](
					myLiftFunction[JString, Lift.String],
					"A for Lift[A, JString]"
				).map((x, _:c.type) => c.universe.reify(x.splice.values))
				val immediate:Interpolator[c.Expr[String]] = stringBase.toInterpolator
				(jCharsLifted <|> immediate) <~ whitespace.toInterpolator
			}

			val splicableValues:Interpolator[typeclass.Repeated.SplicePiece[c.Expr,(String, JValue)]] = {
				implicit val eitherSplicePiece: typeclass.Eithered[c.type, c.Expr[(String, JValue)], c.Expr[List[(String, JValue)]], typeclass.Repeated.SplicePiece[c.Expr,(String, JValue)]] = typeclass.Eithered.forContext(c).splicePiece

				val keyValue = (liftedKeyValue <~ whitespace.toInterpolator)
				val keyThenValue = (key <~ separator.toInterpolator <~> jvalue.toInterpolator)
					.map((x, _:c.type) => {val (k, v) = x; c.universe.reify(Tuple2.apply(k.splice, v.splice))})
				val mapping = (isString("..").toInterpolator ~> liftedObject <~ whitespace.toInterpolator)
				(keyValue <|> keyThenValue) <|> mapping
			}

			splicableValues
				.repeat(
					delimiter = delimiter.toInterpolator,
					strategy = RepeatStrategy.Possessive,
				)(using typeclass.Repeated.contextFromSplicesToExprList[c.type, (String, JValue)])
				.map((x, _) => c.universe.reify(JObject.apply(x.splice)))
		}

		val extractor:Extractor[c.Expr[JObject]] = {
			ofType[(String, JValue)].toExtractor
				.repeat[c.Expr[List[(String, JValue)]]](delimiter = delimiter.toExtractor)
				.contramap((x, _) => c.universe.reify(x.splice.obj))
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
				{(value, _) => value},
				PartialExprFunction(
					{(value:c.Expr[JValue], _:c.type) => isInstanceOfTree(typ)(value)},
					{(value:c.Expr[JValue], _:c.type) => asInstanceOfTree(typ)(value)},
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
