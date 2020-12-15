package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
import scala.quoted.{Expr, Quotes, Type}
import scalajson.ast._
import com.rayrobdod.stringContextParserCombinator._

object MacroImpl {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	def concatenateStrings(strings:Seq[Expr[String]])(using Quotes):Expr[String] = {
		strings match {
			case Seq() => '{ "" }
			case Seq(x) => x
			case _ => '{ ${Expr.ofSeq(strings)}.mkString }
		}
	}

	trait CollectionAssembly[A, CC] {
		type Builder
		def builderType(using Quotes):Type[Builder]
		def newBuilder(using Quotes):Expr[Builder]
		def insertOne(builder:Expr[Builder], item:Expr[A])(using Quotes):Expr[Builder]
		def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[A]])(using Quotes):Expr[Builder]
		def result(builder:Expr[Builder])(using Quotes):Expr[CC]
	}

	object VectorCollectionAssembly extends CollectionAssembly[JValue, Vector[JValue]] {
		type Builder = scala.collection.mutable.Builder[JValue, Vector[JValue]]
		override def builderType(using Quotes):Type[Builder] = implicitly[Type[scala.collection.mutable.Builder[JValue, Vector[JValue]]]]
		override def newBuilder(using Quotes):Expr[Builder] = '{ Vector.newBuilder[JValue] }
		override def insertOne(builder:Expr[Builder], item:Expr[JValue])(using Quotes):Expr[Builder] = '{ $builder.addOne($item) }
		override def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[JValue]])(using Quotes):Expr[Builder] = '{ $builder.addAll($items) }
		override def result(builder:Expr[Builder])(using Quotes):Expr[Vector[JValue]] = '{ $builder.result() }
	}

	object MapCollectionAssembly extends CollectionAssembly[(String, JValue), Map[String, JValue]] {
		type Builder = scala.collection.mutable.Builder[(String, JValue), Map[String, JValue]]
		override def builderType(using Quotes):Type[Builder] = implicitly[Type[Builder]]
		override def newBuilder(using Quotes):Expr[Builder] = '{ Map.newBuilder[String, JValue] }
		override def insertOne(builder:Expr[Builder], item:Expr[(String, JValue)])(using Quotes):Expr[Builder] = '{ $builder.addOne($item) }
		override def insertMany(builder:Expr[Builder], items:Expr[TraversableOnce[(String, JValue)]])(using Quotes):Expr[Builder] = '{ $builder.addAll($items) }
		override def result(builder:Expr[Builder])(using Quotes):Expr[Map[String, JValue]] = '{ $builder.result() }
	}

	def assembleCollection[A, CC]
			(assembly:CollectionAssembly[A, CC])
			(parts:List[Either[Expr[A], Expr[TraversableOnce[A]]]])
			(using c:Quotes, ta:Type[A], tc:Type[CC])
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
	def myLiftFunction[Z, Lifter[A] <: Lift[A, Z]](using Type[Lifter], Type[Z]):LiftFunction[Lifter, Z] = {
		new LiftFunction[Lifter, Z] {
			def apply[A](lifter:Expr[Lifter[A]], a:Expr[A])(using Quotes, Type[A]):Expr[Z] = {
				'{ $lifter.apply($a) }
			}
		}
	}

	def stringContext_json(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[JValue] = {
		// ArrayP, ObjectP and ValueP are mutually recursive; if they were not in an object
		// there would be problems about `ValueP forward reference extends over definition of value ArrayP`
		object ParserPieces extends Parsers {
			val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat().map(_ => ())

			val NullP:Parser[Expr[JNull.type]] = IsString("null").map(_ => '{ scalajson.ast.JNull })

			val BooleanP:Parser[Expr[JBoolean]] = {
				val TrueI = IsString("true").map(_ => '{ scalajson.ast.JTrue })
				val FalseI = IsString("false").map(_ => '{ scalajson.ast.JFalse })
				val LiftedV = Lifted[Lift.Boolean, JBoolean](
					new TypeFunction[Lift.Boolean]{def apply[A](inType:Type[A])(using Quotes) = { '[ Lift[$inType, JBoolean]] }},
					myLiftFunction[JBoolean, Lift.Boolean],
					Expecting("A for Lift[A, JBoolean]")
				)
				LiftedV orElse TrueI orElse FalseI
			}

			val NumberP:Parser[Expr[JNumber]] = {
				import scala.Predef.charWrapper

				val NumberI:Parser[Expr[JNumber]] = {
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
						'{ scalajson.ast.JNumber.fromString( ${Expr[String](x)} ).get }
					})
				}.opaque(Expecting("Number Literal"))
				val AstV:Parser[Expr[JNumber]] = OfType[JNumber]
				val LiftedV = Lifted[Lift.Number, JNumber](
					new TypeFunction[Lift.Number]{def apply[A](inType:Type[A])(using Quotes) = { '[ Lift[$inType, JNumber]] }},
					myLiftFunction[JNumber, Lift.Number],
					Expecting("A for Lift[A, JNumber]")
				)
				AstV orElse LiftedV orElse NumberI
			}

			val StringBase:Parser[Expr[String]] = {
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
				val JCharsI:Parser[Expr[String]] = JCharP.repeat(1).map(Expr.apply _)
				val ScalaVInner:Parser[Expr[String]] = OfType[String]
				val AstVInner:Parser[Expr[String]] = OfType[JString].map(x => '{ $x.value })
				val Content:Parser[Expr[String]] = (AstVInner orElse ScalaVInner orElse JCharsI).repeat()
					.map(strs => concatenateStrings(strs))
				(DelimiterP andThen Content andThen DelimiterP)
			}

			val StringP:Parser[Expr[String]] = {
				val ScalaVOuter:Parser[Expr[String]] = OfType[String]
				val AstVOuter:Parser[Expr[String]] = OfType[JString].map(x => '{ $x.value })
				val Immediate:Parser[Expr[String]] = StringBase
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			val JStringP:Parser[Expr[JString]] = {
				val ScalaVOuter:Parser[Expr[JString]] = OfType[String].map(x => '{ scalajson.ast.JString.apply($x) })
				val AstVOuter:Parser[Expr[JString]] = OfType[JString]
				val Immediate:Parser[Expr[JString]] = StringBase.map(x => '{ scalajson.ast.JString.apply($x)})
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			/** An AndThenTypes that melds the shape of an A followed by a repeating A into a single List */
			implicit def headTailSequenced[A]:typelevel.Sequenced[A, Seq[A], List[A]] = new HeadTailSequenced
			private[this] final class HeadTailSequenced[A] extends typelevel.Sequenced[A, Seq[A], List[A]] {
				def aggregate(a:A, bs:Seq[A]):List[A] = a :: bs.toList
			}

			val ArrayP:Parser[Expr[JArray]] = DelayedConstruction(() => {
				val Prefix:Parser[Unit] = IsString("[")
				val Delim:Parser[Unit] = IsString(",")
				val Suffix:Parser[Unit] = IsString("]")

				val LiftedArrayV = Lifted[Lift.Array, JArray](
					new TypeFunction[Lift.Array]{def apply[A](inType:Type[A])(using Quotes) = { '[ Lift[$inType, JArray]] }},
					myLiftFunction[JArray, Lift.Array],
					Expecting("A for Lift[A, JArray]")
				)
				val LiftedArrayV2 = LiftedArrayV.map(x => '{ $x.value })

				val SplicableValue:Parser[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]] = (
					ValueP.map(x => Left(x)) orElse
					(WhitespaceP andThen IsString("..") andThen LiftedArrayV2
						andThen WhitespaceP).map(x => Right(x))
				)
				val LiteralPresplice:Parser[List[Either[Expr[JValue], Expr[TraversableOnce[JValue]]]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						((SplicableValue andThen (Delim andThen SplicableValue).repeat()) andThen Suffix)
					))
				)
				val Literal:Parser[Expr[JArray]] = (
					LiteralPresplice
						.map(xs => assembleCollection(VectorCollectionAssembly)(xs))
						.map(x => '{ JArray.apply($x)})
				)

				LiftedArrayV orElse Literal
			})

			val ObjectP:Parser[Expr[JObject]] = DelayedConstruction(() => {
				val Prefix:Parser[Unit] = IsString("{")
				val Separator:Parser[Unit] = IsString(":")
				val Delim:Parser[Unit] = IsString(",")
				val Suffix:Parser[Unit] = IsString("}")

				val ObjectV = Lifted[Lift.Object, JObject](
					new TypeFunction[Lift.Object]{def apply[A](inType:Type[A])(using Quotes) = { '[ Lift[$inType, JObject]] }},
					myLiftFunction[JObject, Lift.Object],
					Expecting("A for Lift[A, JObject]")
				)
				val ObjectV2 = ObjectV.map(x => '{ $x.value })

				val KeyValueV = Lifted[Lift.KeyValue, (java.lang.String, JValue)](
					new TypeFunction[Lift.KeyValue]{def apply[A](inType:Type[A])(using Quotes) = { '[ Lift[$inType, (java.lang.String, JValue)]] }},
					myLiftFunction[(java.lang.String, JValue), Lift.KeyValue],
					Expecting("A for Lift[A, (String, JValue)]")
				)

				val KeyV = WhitespaceP andThen StringP andThen WhitespaceP


				val SplicableValue:Parser[Either[Expr[(String, JValue)], Expr[TraversableOnce[(String, JValue)]]]] = (
					(WhitespaceP andThen KeyValueV andThen WhitespaceP)
						.map(x => Left(x)) orElse
					(KeyV andThen Separator andThen ValueP)
						.map(x => {val (k, v) = x; '{ Tuple2.apply($k, $v) }})
						.map(x => Left(x)) orElse
					(WhitespaceP andThen IsString("..") andThen ObjectV2 andThen WhitespaceP)
						.map(x => Right(x))
				)
				val LiteralPresplice:Parser[List[Either[Expr[(String, JValue)], Expr[TraversableOnce[(String, JValue)]]]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						((SplicableValue andThen (Delim andThen SplicableValue).repeat()) andThen Suffix)
					))
				)
				val Literal:Parser[Expr[JObject]] = (
					LiteralPresplice
						.map(xs => assembleCollection(MapCollectionAssembly)(xs))
						.map(x => '{ JObject.apply($x) })
				)

				ObjectV orElse Literal
			})

			val ValueP:Parser[Expr[JValue]] = {
				(WhitespaceP andThen (
					NullP orElse BooleanP orElse NumberP orElse JStringP orElse ArrayP orElse ObjectP
				) andThen WhitespaceP)
			}

			val Aggregate = (ValueP andThen End())
		}

		macroimpl(ParserPieces.Aggregate)(sc, args)
	}
}
