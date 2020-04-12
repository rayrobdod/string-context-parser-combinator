package com.rayrobdod.stringContextParserCombinatorExample.json

import scala.collection.immutable.{Map, Seq, Vector}
import scalajson.ast._
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

object MacroImpl {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	def concatenateStrings(c:Context)(strings:Seq[c.Expr[String]]):c.Expr[String] = {
		strings match {
			case Seq() => c.universe.reify("")
			case Seq(x) => x
			case _ => {
				val accumulatorName = MacroCompat.newTermName(c)("accumulator$")
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
								MacroCompat.stdTermNames(c).CONSTRUCTOR
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

	trait CollectionAssembly[A, CC] {
		type Builder
		def builderType(c:Context):c.universe.TypeTag[Builder]
		def newBuilder(c:Context):c.Expr[Builder]
		def insertOne(c:Context)(builder:c.Expr[Builder], item:c.Expr[A]):c.Expr[_]
		def insertMany(c:Context)(builder:c.Expr[Builder], items:c.Expr[TraversableOnce[A]]):c.Expr[_]
		def result(c:Context)(builder:c.Expr[Builder]):c.Expr[CC]
	}

	object VectorCollectionAssembly extends CollectionAssembly[JValue, Vector[JValue]] {
		type Builder = scala.collection.mutable.Builder[JValue, Vector[JValue]]
		override def builderType(c:Context):c.universe.TypeTag[Builder] = c.universe.typeTag[scala.collection.mutable.Builder[JValue, Vector[JValue]]]
		override def newBuilder(c:Context):c.Expr[Builder] = c.universe.reify(Vector.newBuilder)
		override def insertOne(c:Context)(builder:c.Expr[Builder], item:c.Expr[JValue]):c.Expr[_] = c.universe.reify(builder.splice.+=(item.splice))
		override def insertMany(c:Context)(builder:c.Expr[Builder], items:c.Expr[TraversableOnce[JValue]]):c.Expr[_] = c.universe.reify(builder.splice.++=(items.splice))
		override def result(c:Context)(builder:c.Expr[Builder]):c.Expr[Vector[JValue]] = c.universe.reify(builder.splice.result)
	}

	object MapCollectionAssembly extends CollectionAssembly[(String, JValue), Map[String, JValue]] {
		type Builder = scala.collection.mutable.Builder[(String, JValue), Map[String, JValue]]
		override def builderType(c:Context):c.universe.TypeTag[Builder] = c.universe.typeTag[Builder]
		override def newBuilder(c:Context):c.Expr[Builder] = c.universe.reify(Map.newBuilder)
		override def insertOne(c:Context)(builder:c.Expr[Builder], item:c.Expr[(String, JValue)]):c.Expr[_] = c.universe.reify(builder.splice.+=(item.splice))
		override def insertMany(c:Context)(builder:c.Expr[Builder], items:c.Expr[TraversableOnce[(String, JValue)]]):c.Expr[_] = c.universe.reify(builder.splice.++=(items.splice))
		override def result(c:Context)(builder:c.Expr[Builder]):c.Expr[Map[String, JValue]] = c.universe.reify(builder.splice.result)
	}

	def assembleCollection[A, CC](c:Context)(assembly:CollectionAssembly[A, CC])(parts:List[Either[c.Expr[A], c.Expr[TraversableOnce[A]]]]):c.Expr[CC] = {
		val builderName = MacroCompat.freshName(c)(MacroCompat.newTermName(c)("builder"))
		val builderType = assembly.builderType(c)
		val builderTypeTree = c.universe.TypeTree(builderType.tpe)
		val builderExpr = c.Expr(c.universe.Ident(builderName))(builderType)

		val createBuilder = c.universe.ValDef(
			c.universe.NoMods,
			builderName,
			builderTypeTree,
			assembly.newBuilder(c).tree
		)
		val insertBuilder = parts.map(part => part match {
			case Left(single) => assembly.insertOne(c)(builderExpr, single).tree
			case Right(group) => assembly.insertMany(c)(builderExpr, group).tree
		})

		c.Expr[CC](
			c.universe.Block(
				createBuilder :: insertBuilder,
				assembly.result(c)(builderExpr).tree
			)
		)
	}

	import scala.language.higherKinds
	def myLiftFunction[Z, Lifter[A] <: Lift[A, Z]](c:Context):LiftFunction[c.type, Lifter, Z] = {
		new LiftFunction[c.type, Lifter, Z] {
			def apply[A](lifter:c.Expr[Lifter[A]], a:c.Expr[A]):c.Expr[Z] = {
				c.Expr(
					c.universe.Apply(
						c.universe.Select(
							lifter.tree,
							MacroCompat.newTermName(c)("apply")
						),
						List(a.tree)
					)
				)
			}
		}
	}

	def stringContext_json(c:Context {type PrefixType = JsonStringContext})(args:c.Expr[Any]*):c.Expr[JValue] = {
		// ArrayP, ObjectP and ValueP are mutually recursive; if they were not in an object
		// there would be problems about `ValueP forward reference extends over definition of value ArrayP`
		object ParserPieces extends Parsers {
			val ctx:c.type = c

			// not just so that the type is only computed once; around JArray, it suddenly looses its Lift TypeTag
			val liftTypeConstructor = c.typeOf[Lift[_,_]].typeConstructor

			val WhitespaceP:Parser[Unit] = CharIn("\n\r\t ").repeat().map(_ => ())

			val NullP:Parser[c.Expr[JNull.type]] = IsString("null").map(_ => c.universe.reify(scalajson.ast.JNull))

			val BooleanP:Parser[c.Expr[JBoolean]] = {
				val TrueI = IsString("true").map(_ => c.universe.reify(scalajson.ast.JTrue))
				val FalseI = IsString("false").map(_ => c.universe.reify(scalajson.ast.JFalse))
				val LiftedV = Lifted[Lift.Boolean, JBoolean](
					inType => c.universe.appliedType(liftTypeConstructor, List(inType, c.typeOf[JBoolean])),
					myLiftFunction[JBoolean, Lift.Boolean](c),
					Failure.Leaf("A for Lift[A, JBoolean]")
				)
				LiftedV orElse TrueI orElse FalseI
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
						val xExpr = c.Expr[String](c.universe.Literal(c.universe.Constant(x)))
						c.universe.reify(scalajson.ast.JNumber.fromString(xExpr.splice).get)
					})
				}.opaque("Number Literal")
				val AstV:Parser[c.Expr[JNumber]] = OfType(c.typeTag[JNumber])
				val LiftedV = Lifted[Lift.Number, JNumber](
					inType => c.universe.appliedType(liftTypeConstructor, List(inType, c.typeOf[JNumber])),
					myLiftFunction[JNumber, Lift.Number](c),
					Failure.Leaf("A for Lift[A, JNumber]")
				)
				AstV orElse LiftedV orElse NumberI
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
				val AstVInner:Parser[c.Expr[String]] = OfType(c.typeTag[JString]).map(x => c.universe.reify(x.splice.value))
				val Content:Parser[c.Expr[String]] = (AstVInner orElse ScalaVInner orElse JCharsI).repeat()
					.map(strs => concatenateStrings(c)(strs))
				(DelimiterP andThen Content andThen DelimiterP)
			}

			val StringP:Parser[c.Expr[String]] = {
				val ScalaVOuter:Parser[c.Expr[String]] = OfType(c.typeTag[String])
				val AstVOuter:Parser[c.Expr[String]] = OfType(c.typeTag[JString]).map(x => c.universe.reify(x.splice.value))
				val Immediate:Parser[c.Expr[String]] = StringBase
				AstVOuter orElse ScalaVOuter orElse Immediate
			}

			val JStringP:Parser[c.Expr[JString]] = {
				val ScalaVOuter:Parser[c.Expr[JString]] = OfType(c.typeTag[String]).map(x => c.universe.reify(scalajson.ast.JString.apply(x.splice)))
				val AstVOuter:Parser[c.Expr[JString]] = OfType(c.typeTag[JString])
				val Immediate:Parser[c.Expr[JString]] = StringBase.map(x => c.universe.reify(scalajson.ast.JString.apply(x.splice)))
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

				val LiftedArrayV = Lifted[Lift.Array, JArray](
					inType => c.universe.appliedType(liftTypeConstructor, List(inType, c.typeOf[JArray])),
					myLiftFunction[JArray, Lift.Array](c),
					Failure.Leaf("A for Lift[A, JArray]")
				)
				val LiftedArrayV2 = LiftedArrayV.map(x => c.Expr[Vector[JValue]](c.universe.Select(x.tree, MacroCompat.newTermName(c)("value"))))

				val SplicableValue:Parser[Either[c.Expr[JValue], c.Expr[TraversableOnce[JValue]]]] = (
					ValueP.map(x => Left(x)) orElse
					(WhitespaceP andThen IsString("..") andThen LiftedArrayV2
						andThen WhitespaceP).map(x => Right(x))
				)
				val LiteralPresplice:Parser[List[Either[c.Expr[JValue], c.Expr[TraversableOnce[JValue]]]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						((SplicableValue andThen (Delim andThen SplicableValue).repeat()) andThen Suffix)
					))
				)
				val Literal:Parser[c.Expr[JArray]] = (
					LiteralPresplice
						.map(xs => assembleCollection(c)(VectorCollectionAssembly)(xs))
						.map(x => c.universe.reify(JArray.apply(x.splice)))
				)

				LiftedArrayV orElse Literal
			})

			val ObjectP:Parser[c.Expr[JObject]] = DelayedConstruction(() => {
				val Prefix:Parser[Unit] = IsString("{")
				val Separator:Parser[Unit] = IsString(":")
				val Delim:Parser[Unit] = IsString(",")
				val Suffix:Parser[Unit] = IsString("}")

				val ObjectV = Lifted[Lift.Object, JObject](
					inType => c.universe.appliedType(liftTypeConstructor, List(inType, c.typeOf[JObject])),
					myLiftFunction[JObject, Lift.Object](c),
					Failure.Leaf("A for Lift[A, JObject]")
				)
				val ObjectV2 = ObjectV.map(x => c.Expr[Map[String, JValue]](c.universe.Select(x.tree, MacroCompat.newTermName(c)("value"))))

				val KeyValueV = Lifted[Lift.KeyValue, (java.lang.String, JValue)](
					inType => c.universe.appliedType(liftTypeConstructor, List(inType, c.typeOf[(java.lang.String, JValue)])),
					myLiftFunction[(java.lang.String, JValue), Lift.KeyValue](c),
					Failure.Leaf("A for Lift[A, (String, JValue)]")
				)

				val KeyV = WhitespaceP andThen StringP andThen WhitespaceP


				val SplicableValue:Parser[Either[c.Expr[(String, JValue)], c.Expr[TraversableOnce[(String, JValue)]]]] = (
					(WhitespaceP andThen KeyValueV andThen WhitespaceP)
						.map(x => Left(x)) orElse
					(KeyV andThen Separator andThen ValueP)
						.map(x => {val (k, v) = x; c.universe.reify(Tuple2.apply(k.splice, v.splice))})
						.map(x => Left(x)) orElse
					(WhitespaceP andThen IsString("..") andThen ObjectV2 andThen WhitespaceP)
						.map(x => Right(x))
				)
				val LiteralPresplice:Parser[List[Either[c.Expr[(String, JValue)], c.Expr[TraversableOnce[(String, JValue)]]]]] = (
					(Prefix andThen WhitespaceP andThen (
						Suffix.map(_ => List.empty) orElse
						((SplicableValue andThen (Delim andThen SplicableValue).repeat()) andThen Suffix)
					))
				)
				val Literal:Parser[c.Expr[JObject]] = (
					LiteralPresplice
						.map(xs => assembleCollection(c)(MapCollectionAssembly)(xs))
						.map(x => c.universe.reify(JObject.apply(x.splice)))
				)

				ObjectV orElse Literal
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
