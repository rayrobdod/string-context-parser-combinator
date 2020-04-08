package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
import scalajson.ast._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinator.MacroCompat
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context
import com.rayrobdod.stringContextParserCombinator.Utilities._

object MacroImpl {
	def stringContext_json(c:Context {type PrefixType = JsonStringContext})(args:c.Expr[Any]*):c.Expr[JValue] = {
		// ArrayP, ObjectP and ValueP are mutually recursive; if they were not in an object
		// there would be problems about `ValueP forward reference extends over definition of value ArrayP`
		object ParserPieces extends Parsers {
			type ContextType = c.type

			val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat().map(_ => ())

			val NullP:Parser[c.Expr[JNull.type]] = IsString("null").map(_ => c.universe.reify(scalajson.ast.JNull))

			val BooleanP:Parser[c.Expr[JBoolean]] = {
				val TrueI = IsString("true").map(_ => c.universe.reify(scalajson.ast.JTrue))
				val FalseI = IsString("false").map(_ => c.universe.reify(scalajson.ast.JFalse))
				val ScalaV = OfType(c.typeTag[scala.Boolean]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JBoolean).tree, "apply", List(x.tree))))
				val AstV = OfType(c.typeTag[JBoolean])
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
					implicit object CharStringOptionallyTypes extends Implicits.OptionallyTypes[Char, String] {
						def none():String = ""
						def some(elem:Char):String = elem.toString
					}
					implicit object StringStringOptionallyTypes extends Implicits.OptionallyTypes[String, String] {
						def none():String = ""
						def some(elem:String):String = elem
					}

					(
						CharIn("-").optionally
						andThen (CharIn("0").map(_.toString) orElse (CharIn('1' to '9').map(_.toString) andThen RepeatedDigits(0)))
						andThen (CharIn(".").map(_.toString) andThen RepeatedDigits(1)).optionally
						andThen (CharIn("eE").map(_.toString) andThen CharIn("+-").optionally andThen RepeatedDigits(1)).optionally
					).map({x =>
						c.Expr(c.universe.Select(
							objectApply(c)(c.universe.reify(scalajson.ast.JNumber).tree, "fromString", List(c.universe.Literal(c.universe.Constant(x)))),
							MacroCompat.newTermName(c)("get")
						))
					})
				}.opaque("Number Literal")
				val AstV:Parser[c.Expr[JNumber]] = OfType(c.typeTag[JNumber])
				val ScalaIntV:Parser[c.Expr[JNumber]] = OfType(c.typeTag[scala.Int]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JNumber).tree, "apply", List(x.tree))))
				AstV orElse ScalaIntV orElse NumberI
			}

			val StringBase:Parser[c.Expr[String]] = {
				val DelimiterP:Parser[Unit] = IsString("\"")
				val JCharImmediate:Parser[Char] = CharWhere(c => c >= ' ' && c != '"' && c != '\\', "printable character other than '\"' or '\\'")
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
				val JCharsI:Parser[c.Expr[String]] = JCharP.repeat(1).map(x => c.Expr(c.universe.Literal(c.universe.Constant(x))))
				val ScalaVInner:Parser[c.Expr[String]] = OfType(c.typeTag[String])
				val AstVInner:Parser[c.Expr[String]] = OfType(c.typeTag[JString]).map(x => c.Expr(c.universe.Select(x.tree, MacroCompat.newTermName(c)("value"))))
				val Content:Parser[c.Expr[String]] = (AstVInner orElse ScalaVInner orElse JCharsI).repeat()
					.map(strs => Utilities.concatenateStrings(c)(strs))
				(DelimiterP andThen Content andThen DelimiterP)
			}

			val StringP:Parser[c.Expr[String]] = {
				val ScalaVOuter:Parser[c.Expr[String]] = OfType(c.typeTag[String])
				val AstVOuter:Parser[c.Expr[String]] = OfType(c.typeTag[JString]).map(x => c.Expr(c.universe.Select(x.tree, MacroCompat.newTermName(c)("value"))))
				val Immediate:Parser[c.Expr[String]] = StringBase.map(x => c.Expr(x.tree))
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			val JStringP:Parser[c.Expr[JString]] = {
				val ScalaVOuter:Parser[c.Expr[JString]] = OfType(c.typeTag[String]).map(x => c.Expr(objectApply(c)(c.universe.reify(scalajson.ast.JString).tree, "apply", List(x.tree))))
				val AstVOuter:Parser[c.Expr[JString]] = OfType(c.typeTag[JString])
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
				val ScalaV:Parser[c.Expr[Vector[JValue]]] = OfType(c.typeTag[Vector[JValue]])
				val VectorP:Parser[c.Expr[Vector[JValue]]] = ScalaV orElse Elems2
				val JArrayP:Parser[c.Expr[JArray]] = VectorP.map(x => c.Expr(objectApply(c)(c.universe.reify(JArray).tree, "apply", List(x.tree))))
				val AstV:Parser[c.Expr[JArray]] = OfType(c.typeTag[JArray])
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
				val ScalaV:Parser[c.Expr[Map[String, JValue]]] = OfType(c.typeTag[Map[String, JValue]])
				val VectorP:Parser[c.Expr[Map[String, JValue]]] = ScalaV orElse Elems2
				val JObjectP:Parser[c.Expr[JObject]] = VectorP.map(x => c.Expr(objectApply(c)(c.universe.reify(JObject).tree, "apply", List(x.tree))))
				val AstV:Parser[c.Expr[JObject]] = OfType(c.typeTag[JObject])
				AstV orElse JObjectP
			})

			val ValueP:Parser[c.Expr[JValue]] = {
				(WhitespaceP andThen (
					NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP
				) andThen WhitespaceP).map({x => c.Expr(x.tree)})
			}

			val Aggregate = (ValueP andThen End())
		}

		val className = "com.rayrobdod.stringContextParserCombinatorExample.json.package.JsonStringContext"
		macroimpl(c)(className, ParserPieces.Aggregate)(args.toList)
	}
}
