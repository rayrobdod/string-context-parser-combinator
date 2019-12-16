package com.rayrobdod.stringContextParserCombinator
package example.json

import scala.collection.immutable.{Map, Seq, Vector}
import scala.reflect.macros.blackbox.Context
import scalajson.ast._
import com.rayrobdod.stringContextParserCombinator.Utilities._

object MacroImpl {
	def stringContext_json(c:Context {type PrefixType = JsonStringContext})(args:c.Expr[Any]*):c.Expr[JValue] = {
		val self:c.Expr[JsonStringContext] = c.prefix

		/* Extract the string context `parts`, to be used in the parse */

		val JsonStringContextName = decodedName("JsonStringContext")
		val JsonSelectChain = selectChain[c.type](c, "com.rayrobdod.stringContextParserCombinator.example.json")
		val StringContextApply = stringContextApply[c.type](c)

		import c.universe._ // ApplyTag, SelectTag etc.
		val strings = self.tree.duplicate match {
			case c.universe.Apply(
				c.universe.Select(
					c.universe.Select(
						JsonSelectChain(),
						c.universe.nme.PACKAGE
					),
					JsonStringContextName()
				),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (evalSimple(c)(x), PositionPoint(x.tree.pos))})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(self))
		}

		/* Create the input to parse */

		val input = Input[c.type](strings, args.toList)

		/* Create the parser */

		// ArrayP, ObjectP and ValueP are mutually recursive; if they were not in an object
		// there would be problems about `ValueP forward reference extends over definition of value ArrayP`
		object ParserPieces {
			// The scala compiler is not great at dealing with `c.type` as a type parameter.
			// It seems to prefer to infer `Context` than `c.type`, despite `c.type` being
			// necessary to handle `c.Expr` values; `Context#Expr` just won't work
			// Shadow package factory methods with these factory methods to reduce the number of times I have to type `c.type`
			type Parser[A] = com.rayrobdod.stringContextParserCombinator.Parser[c.type, A]
			def CharIn(str:Seq[Char]) = new com.rayrobdod.stringContextParserCombinator.CharIn[c.type](str)
			def CharIn(str:String) = new com.rayrobdod.stringContextParserCombinator.CharIn[c.type](scala.Predef.wrapString(str))
			def CharWhere(fn:Function1[Char, Boolean]) = new com.rayrobdod.stringContextParserCombinator.CharWhere[c.type](fn)
			def IsString(str:String) = new com.rayrobdod.stringContextParserCombinator.IsString[c.type](str)
			def OfType(tpe:c.Type) = new com.rayrobdod.stringContextParserCombinator.OfType[c.type](tpe)
			def End() = new com.rayrobdod.stringContextParserCombinator.End[c.type]()
			def DelayedConstruction[A](fn:Function0[Parser[A]]) = new com.rayrobdod.stringContextParserCombinator.DelayedConstruction[c.type, A](fn)


			val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat().map(_ => ())

			val NullP:Parser[c.Expr[JNull.type]] = IsString("null").map(_ => c.universe.reify(scalajson.ast.JNull))

			val BooleanP:Parser[c.Expr[JBoolean]] = {
				val TrueI = IsString("true").map(_ => c.universe.reify(scalajson.ast.JTrue))
				val FalseI = IsString("false").map(_ => c.universe.reify(scalajson.ast.JFalse))
				val ScalaV = OfType(c.typeOf[scala.Boolean]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JBoolean).tree, "apply", List(x))))
				val AstV = OfType(c.typeOf[JBoolean]).map(x => c.Expr(x))
				AstV orElse ScalaV orElse TrueI orElse FalseI
			}

			val NumberP:Parser[c.Expr[JNumber]] = {
				import scala.Predef.charWrapper

				val NumberI:Parser[c.Expr[JNumber]] = {
					def RepeatedDigits(min:Int):Parser[String] = CharIn('0' to '9').repeat(min)

					/* Concatenate every capture in the following parser and combine into one long string */
					implicit object StringStringAndThenTypes extends Implicits.AndThenTypes[String, String, String] {
						def aggregate(a:String, b:String):String = a + b
					}
					implicit object StringStringRepeatTypes extends Implicits.RepeatTypes[String, String] {
						type Acc = StringBuilder
						def init():Acc = new StringBuilder
						def append(acc:Acc, elem:String):Unit = {acc ++= elem}
						def result(acc:Acc):String = acc.toString
					}

					(
						CharIn("-").repeat(0, 1)
						andThen (CharIn("0").map(_.toString) orElse (CharIn('1' to '9').map(_.toString) andThen RepeatedDigits(0)))
						andThen (CharIn(".").map(_.toString) andThen RepeatedDigits(1)).repeat(0, 1)
						andThen (CharIn("eE").map(_.toString) andThen CharIn("+-").repeat(0, 1) andThen RepeatedDigits(1)).repeat(0, 1)
					).map({x =>
						c.Expr(c.universe.Select(
							objectApply(c)(c.universe.reify(scalajson.ast.JNumber).tree, "fromString", List(c.literal(x).tree)),
							c.universe.newTermName("get")
						))
					})
				}.opaque("Number Literal")
				val AstV:Parser[c.Expr[JNumber]] = OfType(c.typeOf[JNumber]).map(x => c.Expr(x))
				val ScalaIntV:Parser[c.Expr[JNumber]] = OfType(c.typeOf[scala.Int]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JNumber).tree, "apply", List(x))))
				AstV orElse ScalaIntV orElse NumberI
			}

			val StringBase:Parser[c.Expr[String]] = {
				val DelimiterP:Parser[Unit] = IsString("\"")
				val JCharImmediate:Parser[Char] = CharWhere(c => c >= ' ' && c != '"' && c != '\\')
				val JCharEscaped:Parser[Char] = (
					(IsString("\\") andThen (
						CharIn("\\/\"") orElse
						IsString("n").map(_ => '\n') orElse
						IsString("r").map(_ => '\r') orElse
						IsString("b").map(_ => '\b') orElse
						IsString("f").map(_ => '\f') orElse
						IsString("t").map(_ => '\t')
					))
				)
				val JCharP:Parser[Char] = JCharEscaped orElse JCharImmediate
				val JCharsI:Parser[c.Expr[String]] = JCharP.repeat(1).map(x => c.literal(x))
				val ScalaVInner:Parser[c.Expr[String]] = OfType(c.typeOf[String]).map(x => c.Expr(x))
				val AstVInner:Parser[c.Expr[String]] = OfType(c.typeOf[JString]).map(x => c.Expr(c.universe.Select(x, c.universe.newTermName("value"))))
				val Content:Parser[c.Expr[String]] = (AstVInner orElse ScalaVInner orElse JCharsI).repeat().map({x =>
					val summedString:c.Tree = x match {
						case Seq() => c.literal("").tree
						case Seq(x) => x.tree
						case xs:Seq[_] => {
							val accumulatorName = c.universe.newTermName("accumulator$")
							val accumulatorType = c.universe.Select(
								c.universe.Select(
									c.universe.Select(
										c.universe.Ident(c.universe.newTermName("scala")),
										c.universe.newTermName("collection")
									),
									c.universe.newTermName("mutable")
								),
								c.universe.newTypeName("StringBuilder")
							)
							val stats = scala.collection.mutable.Buffer[c.universe.Tree](
								c.universe.ValDef(
									c.universe.NoMods,
									accumulatorName,
									accumulatorType,
									c.universe.Apply(
										c.universe.Select(
											c.universe.New(accumulatorType),
											c.universe.nme.CONSTRUCTOR
										),
										List()
									)
								)
							)
							xs.map(_.asInstanceOf[c.Expr[String]]).foreach({x => stats += objectApply(c)(c.universe.Ident(accumulatorName), "append", List(x.tree))})
							c.universe.Block(
								stats.toList,
								c.universe.Select(
									c.universe.Ident(accumulatorName),
									c.universe.newTermName("toString")
								)
							)
						}
					}
					c.Expr(summedString)
				})
				(DelimiterP andThen Content andThen DelimiterP)
			}

			val StringP:Parser[c.Expr[String]] = {
				val ScalaVOuter:Parser[c.Expr[String]] = OfType(c.typeOf[String]).map(x => c.Expr(x))
				val AstVOuter:Parser[c.Expr[String]] = OfType(c.typeOf[JString]).map(x => c.Expr(c.universe.Select(x, c.universe.newTermName("value"))))
				val Immediate:Parser[c.Expr[String]] = StringBase.map(x => c.Expr(x.tree))
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			val JStringP:Parser[c.Expr[JString]] = {
				val ScalaVOuter:Parser[c.Expr[JString]] = OfType(c.typeOf[String]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JString).tree, "apply", List(x))))
				val AstVOuter:Parser[c.Expr[JString]] = OfType(c.typeOf[JString]).map(x => c.Expr(x))
				val Immediate:Parser[c.Expr[JString]] = StringBase.map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JString).tree, "apply", List(x.tree))))
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			/** An AndThenTypes that melds the shape of an A followed by a repeating A into a single List */
			implicit def headTailAndThenTypes[A]:Implicits.AndThenTypes[A, Seq[A], List[A]] = new HeadTailAndThenTypes
			private[this] final class HeadTailAndThenTypes[A] extends Implicits.AndThenTypes[A, Seq[A], List[A]] {
				def aggregate(a:A, bs:Seq[A]):List[A] = a :: bs.toList
			}

			val ArrayP:Parser[c.Expr[JArray]] = DelayedConstruction(() => {
				val Prefix:Parser[Unit] = IsString("[")
				val Delim:Parser[Unit] = IsString(",")
				val Suffix:Parser[Unit] = IsString("]")

				val Elems:Parser[List[c.Expr[JValue]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						((ValueP andThen (Delim andThen ValueP).repeat()) andThen Suffix)
					))
				)
				val Elems2:Parser[c.Expr[Vector[JValue]]] = Elems.map(xs => c.Expr(objectApply(c)(c.universe.reify(Vector).tree, "apply", xs.map(_.tree))))
				val ScalaV:Parser[c.Expr[Vector[JValue]]] = OfType(c.typeOf[Vector[JValue]]).map(x => c.Expr(x))
				val VectorP:Parser[c.Expr[Vector[JValue]]] = ScalaV orElse Elems2
				val JArrayP:Parser[c.Expr[JArray]] = VectorP.map(x => c.Expr(objectApply(c)(c.universe.reify(JArray).tree, "apply", List(x.tree))))
				val AstV:Parser[c.Expr[JArray]] = OfType(c.typeOf[JArray]).map(x => c.Expr(x))
				AstV orElse JArrayP
			})

			val ObjectP:Parser[c.Expr[JObject]] = DelayedConstruction(() => {
				val Prefix:Parser[Unit] = IsString("{")
				val Separator:Parser[Unit] = IsString(":")
				val Delim:Parser[Unit] = IsString(",")
				val Suffix:Parser[Unit] = IsString("}")

				val KeyValuePair:Parser[c.Expr[(String, JValue)]] = (
					(WhitespaceP andThen StringP andThen WhitespaceP andThen Separator andThen ValueP).map({x =>
						val (k, v) = x
						c.Expr(objectApply(c)(c.universe.reify(Tuple2).tree, "apply", List(k.tree, v.tree)))
					})
				)

				val Elems:Parser[List[c.Expr[(String, JValue)]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						(KeyValuePair andThen (Delim andThen KeyValuePair).repeat() andThen Suffix)
					))
				)
				val Elems2:Parser[c.Expr[Map[String, JValue]]] = Elems.map(xs => c.Expr(objectApply(c)(c.universe.reify(Map).tree, "apply", xs.map(_.tree))))
				val ScalaV:Parser[c.Expr[Map[String, JValue]]] = OfType(c.typeOf[Map[String, JValue]]).map(x => c.Expr(x))
				val VectorP:Parser[c.Expr[Map[String, JValue]]] = ScalaV orElse Elems2
				val JObjectP:Parser[c.Expr[JObject]] = VectorP.map(x => c.Expr(objectApply(c)(c.universe.reify(JObject).tree, "apply", List(x.tree))))
				val AstV:Parser[c.Expr[JObject]] = OfType(c.typeOf[JObject]).map(x => c.Expr(x))
				AstV orElse JObjectP
			})

			val ValueP:Parser[c.Expr[JValue]] = {
				(WhitespaceP andThen (
					NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP
				) andThen WhitespaceP).map({x => c.Expr(x.tree)})
			}

			val Aggregate = (ValueP andThen End())
		}

		/* Parse the input */

		ParserPieces.Aggregate.parse(input) match {
			case Success(res, _) => {
				//System.out.println(res)
				res
			}
			case f:Failure[_] => {
				f.report(c)
			}
		}
	}
}
