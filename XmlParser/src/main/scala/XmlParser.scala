package name.rayrobdod.stringContextParserCombinatorExample.xml

import scala.collection.immutable.BitSet
import scala.collection.immutable.{Map, Seq}
import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Interpolator => _, _}
import name.rayrobdod.stringContextParserCombinator.Interpolator.*

private[xml] object XmlParser {
	extension [A] (p:Interpolator[A])
		def collect[Z](pf: PartialFunction[(A, Quotes), Z], msg: String): Interpolator[Z] =
			p.filter((a, ctx) => pf.isDefinedAt((a, ctx)), msg).map((a, ctx) => pf.apply((a, ctx)))


	/** Represents a name with a specified short namespace, but an unknown namespace */
	final case class BoundName(prefix:Option[String], local:String)

	sealed trait ThingInAttributePosition

	final case class NamespaceBindingOne(prefix:Option[String], namespace:String) extends ThingInAttributePosition
	type NamespaceBinding = Map[Option[String], String]

	final case class Attribute(key:BoundName, value:List[Expr[Any]]) extends ThingInAttributePosition

	final case class InterpolatedAttribute(data: Expr[Any]) extends ThingInAttributePosition


	private val xmlNameFirstChar:BitSet = {
		val builder = BitSet.newBuilder
		builder ++= 'A'.toInt to 'Z'
		builder += '_'
		builder ++= 'a'.toInt to 'z'
		builder ++= 0x00C0 to 0x00D6
		builder ++= 0x00D8 to 0x00F6
		builder ++= 0x00F8 to 0x02FF
		builder ++= 0x0370 to 0x037D
		builder ++= 0x037F to 0x1FFF
		builder ++= 0x200C to 0x200D
		builder ++= 0x2070 to 0x218F
		builder ++= 0x2C00 to 0x2FEF
		builder ++= 0x3001 to 0xD7FF
		builder ++= 0xF900 to 0xFDCF
		builder ++= 0xFDF0 to 0xFFFD
		builder ++= 0x10000 to 0xEFFFF
		builder.result()
	}
	private val xmlNameRestChar:BitSet = {
		val builder = BitSet.newBuilder
		builder += '-'
		builder += '.'
		builder ++= '0'.toInt to '9'
		builder += 0x00B7
		builder ++= 0x0300 to 0x036F
		builder ++= 0x203F to 0x2040
		builder.result()
	}
	private val xmlAllowedChar:BitSet = {
		val builder = BitSet.newBuilder
		builder += 0x9
		builder += 0xA
		builder += 0xD
		builder ++= 0x20 to 0xD7FF
		builder ++= 0xE000 to 0xFFFD
		builder ++= 0x10000 to 0x10FFFF
		builder.result()
	}

	private val whitespace:Interpolator[Unit] = charIn("\n\r\t ").map(_ => ())

	/* @see https://www.w3.org/TR/xml/#sec-line-ends
	 * 2.11: the XML processor MUST behave as if it normalized all line breaks in external parsed entities
	 */
	private val lineBreak:Interpolator[CodePoint] = ((isString("\r\n") <|> isString("\r")) <|> isString("\n")).map(_ => CodePoint('\n'))

	private def charRef:Interpolator[CodePoint] = {
		def hexInteger = charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(1).map(Integer.parseInt(_, 16))
		def decInteger = charIn('0' to '9').repeat(1).map(Integer.parseInt(_, 10))

		isString("&#") ~> (((isString("x") ~> hexInteger) <|> decInteger) <~ isString(";"))
			.filter(c => xmlAllowedChar(c), "")
			.collect({(c:(Int, Quotes)) => CodePoint(c._1)}.unlift, "")
			.opaque("valid codepoint")
	}

	given typeclass.Sequenced[Quotes, CodePoint, String, String] = typeclass.Sequenced({(head:CodePoint, tail:String, _:Quotes) => s"${head}${tail}"})

	private val xmlNameNoColon:Interpolator[String] = {
		val firstChar = codePointWhere(x => xmlNameFirstChar.contains(x.intValue))
		val restChar = firstChar <|> codePointWhere(x => xmlNameRestChar.contains(x.intValue))
		(firstChar <~> restChar.repeat())
	}
	private val xmlName:Interpolator[String] = {
		val firstChar = codePointIn(":") <|> codePointWhere(x => xmlNameFirstChar.contains(x.intValue))
		val restChar = firstChar <|> codePointWhere(x => xmlNameRestChar.contains(x.intValue))
		(firstChar <~> restChar.repeat())
	}
	private def xmlAllowedChar(exclude:String):Interpolator[CodePoint] = {
		val exclude2:Set[Int] = Set(exclude.codePoints.toArray:_*)
		if exclude2.contains('\n') then
			val bits = ((xmlAllowedChar -- exclude2) - '\r')
			codePointWhere(x => bits.contains(x.intValue))
				.opaque("xml char excluding " + exclude2.mkString("[", ",", "]"))
		else
			val bits = ((xmlAllowedChar -- exclude2) - '\r' - '\n')
			(lineBreak <|> codePointWhere(x => bits.contains(x.intValue)))
				.opaque("xml char excluding " + exclude2.mkString("[", ",", "]"))

	}


	private val boundName:Interpolator[BoundName] = {
		(((xmlNameNoColon <~ isString(":")).attempt.optionally()) <~> xmlName)
			.map(x => BoundName(x._1, x._2))
	}

	private val namespaceBinding:Interpolator[NamespaceBindingOne] = {
		val namespaceValue:Interpolator[String] = {
			((isString("\"") ~> xmlAllowedChar("\"").repeat() <~ isString("\"")) <|>
			(isString("\'") ~> xmlAllowedChar("\'").repeat() <~ isString("\'")))
				.map(x => x.mkString)
		}

		isString("xmlns") ~> (
			(isString("=") ~> namespaceValue).map(x => NamespaceBindingOne(None, x)) <|>
			(isString(":") ~> xmlName <~> isString("=") <~> namespaceValue).map(x => NamespaceBindingOne(Option(x._1), x._2))
		)
	}

	private def interpolation(factory: Expr[XmlFactory[_]]):Interpolator[Expr[_]] = {
		ofType[Any]
			.map({(value) =>
				import quotes.reflect._
				Apply(
					Select.unique(
						factory.asTerm,
						"interpolation",
					),
					List(value.asTerm)
				).asExpr
			})
	}

	private def attribute(factory:Expr[XmlFactory[_]]):Interpolator[Attribute] = {
		def valueText(excluding:Char) = (charRef <|> xmlAllowedChar("<&" + excluding)).repeat(1)
			.map({data =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("values")
					.selectFieldMemberMaybeDynamic(data)
					.asExpr
			})
		def value(excluding:Char) = (entity(factory) <|> valueText(excluding)).repeat(1)

		(
			boundName <~> isString("=") <~> (
					interpolation(factory).map(x => List.apply(x)) <|>
					(isString("\"\"") <|> isString("\'\'")).map(_ => Nil) <|>
					(isString("\"") ~> value('\"') <~ isString("\"")) <|>
					(isString("\'") ~> value('\'') <~ isString("\'"))
				)
		)
			.map({kv => Attribute(kv._1, kv._2)})
	}

	private def cdata(factory:Expr[XmlFactory[_]]):Interpolator[Expr[Any]] = {
		given typeclass.Eithered[Quotes, CodePoint, String, String] with
			def left(elem:CodePoint)(using Quotes):String = elem.toString
			def right(elem:String)(using Quotes):String = elem
		given typeclass.Sequenced[Quotes, CodePoint, CodePoint, String] = typeclass.Sequenced({(head:CodePoint, tail:CodePoint, _: Quotes) => s"${head}${tail}"})
		given typeclass.Sequenced[Quotes, String, CodePoint, String] = typeclass.Sequenced({(head:String, tail:CodePoint, _: Quotes) => s"${head}${tail}"})

		(
			isString("<![CDATA[")
			~> (
				(xmlAllowedChar("]")) <|>
				(codePointIn("]") <~> xmlAllowedChar("]")).attempt <|>
				(codePointIn("]") <~> codePointIn("]") <~> xmlAllowedChar(">")).attempt
			).repeat().map(_.mkString)
			<~ isString("]]>")
		)
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("cdata")
					.selectFieldMemberMaybeDynamic(name)
					.asExpr
			})
	}

	private def entity(factory:Expr[XmlFactory[_]]):Interpolator[Expr[Any]] = {
		(isString("&") ~> xmlName <~ isString(";"))
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("entities")
					.selectFieldMemberMaybeDynamic(name)
					.asExpr
			})
			.attempt
	}

	private def text(factory:Expr[XmlFactory[_]]):Interpolator[Expr[Any]] = {
		(charRef <|> xmlAllowedChar("<&")).repeat(1)
			.map({data =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("texts")
					.selectFieldMemberMaybeDynamic(data)
					.asExpr
			})
	}

	private def comment(factory:Expr[XmlFactory[_]]):Interpolator[Expr[Any]] = {
		(
			isString("<!--")
			~> (
				(xmlAllowedChar("-")) <|>
				(codePointIn("-") <~> xmlAllowedChar("-")).attempt
			).repeat().map(_.mkString)
			<~ isString("-->")
		)
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("comments")
					.selectFieldMemberMaybeDynamic(name)
					.asExpr
			})
	}

	private def processingInstruction(factory:Expr[XmlFactory[_]]):Interpolator[Expr[_]] = {
		(
			isString("<?")
			~> xmlName.filter(_ != "xml", "not `xml`")
			<~> whitespace
			<~> (
				(xmlAllowedChar("?")) <|>
				(codePointIn("?") <~> xmlAllowedChar(">")).attempt
			).repeat().map(_.mkString)
			<~ isString("?>")
		)
			.map({(targetValue) =>
				val (target, value) = targetValue
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("processInstructions")
					.selectAndApplyToArgsMaybeDynamicMaybeVarargs(target)(List(Literal(StringConstant(value))))
					.asExpr
			})
	}

	private def fragment(factory:Expr[XmlFactory[_]], nsb: NamespaceBinding):Interpolator[List[Expr[_]]] = {
		(
			interpolation(factory) <|>
			entity(factory) <|>
			text(factory) <|>
			cdata(factory) <|>
			comment(factory) <|>
			processingInstruction(factory) <|>
			elem(factory, nsb).attempt
		).repeat()
	}

	private def elem(factory:Expr[XmlFactory[_]], nsb:NamespaceBinding):Interpolator[Expr[Any]] = {
		(
			isString("<") ~>
				boundName <~>
				(whitespace.repeat(1) ~> (
					interpolation(factory).map({x => InterpolatedAttribute(x)}) <|>
					namespaceBinding <|>
					attribute(factory)
				)).attempt.repeat() <~
				whitespace.repeat()
			)
			.map({parts =>
				import quotes.reflect._
				val (name, nsAndAttrs:List[ThingInAttributePosition]) = parts
				val nsb2:NamespaceBinding = nsb ++ nsAndAttrs.collect({case NamespaceBindingOne(prefix, ns) => (prefix, ns)})
				val attrs = nsAndAttrs.collect({
					case Attribute(BoundName(ns, localName), value2) =>
						val value = value2.map(_.asTerm)

						val _1 = factory.asTerm
						val _2 = ns match {
							case None => _1
							case Some(prefix) => nsb2.get(Some(prefix)) match {
								case None =>
									_1
										.selectFieldMember("prefixes")
										.selectFieldMemberMaybeDynamic(prefix)
								case Some(uri) =>
									_1
										.selectFieldMember("uris")
										.selectAndApplyToArgsMaybeDynamicMaybeVarargs(uri)(List(Expr(prefix).asTerm))
							}
						}
						_2
								.selectFieldMember("attributes")
								.selectAndApplyToArgsMaybeDynamicMaybeVarargs(localName)(value)
								.asExpr
					case InterpolatedAttribute(x) =>
						x
				})

				(name, attrs, nsb2)
			})
			.flatMap({(parts) =>
				val (elemName, attributes, nsb2) = parts

				(isString("/>").map(_ => List.empty) <|> (
					(isString(">") <~>
					(fragment(factory, nsb2) <~>
					isString("</"))) <~>
					boundName.filter(x => x == elemName, s"$elemName").map(_ => ()) <~>
					whitespace.repeat() <~>
					isString(">")
				))
					.map(children => ((elemName, attributes, nsb2, children)))
			})
			.map({parts =>
				import quotes.reflect._
				val (elemName, attributes, nsb2, children) = parts
				val _1 = factory.asTerm
				val _2 = elemName.prefix match {
					case None => nsb2.get(None) match {
						case None =>
							_1
						case Some("") =>
							_1
						case Some(uri) =>
							_1
								.selectFieldMember("uris")
								.selectFieldMemberMaybeDynamic(uri)
					}
					case Some(prefix) => nsb2.get(Some(prefix)) match {
						case None =>
							_1
								.selectFieldMember("prefixes")
								.selectFieldMemberMaybeDynamic(prefix)
						case Some(uri) =>
							_1
								.selectFieldMember("uris")
								.selectAndApplyToArgsMaybeDynamicMaybeVarargs(uri)(List(Expr(prefix).asTerm))
					}
				}
				_2
					.selectFieldMember("elements")
					.selectAndApplyToArgsMaybeDynamicMaybeVarargs(elemName.local)((attributes ++ children).map(_.asTerm))
					.asExpr
			})
	}


	def interpolate[Z](sc:Expr[scala.StringContext], args:Expr[Seq[Any]], factory:Expr[XmlFactory[Z]])(using Type[Z], Quotes):Expr[Z] = {
		import quotes.reflect.*
		val initialNsb:NamespaceBinding = Map(
			(None, ""),
			(Some("xml"), "http://www.w3.org/XML/1998/namespace"),
			(Some("xmlns"), "http://www.w3.org/200/xmlns/")
		)

		factory.asTerm
			.selectAndApplyToArgsMaybeDynamicMaybeVarargs
				("literal")
				((fragment(factory, initialNsb) <~> end).interpolate(sc, args).map(_.asTerm))
			.asExprOf[Z]
	}

	def classSymbolThatDoesntResultInBadSymbolicReferenceWhenLookingForAnObjectContainedClass(name:String)(using Quotes):quotes.reflect.Symbol = {
		import quotes.reflect.*
		val nameSplit = name.split('.')
		nameSplit.view.init
			.foldLeft[quotes.reflect.Symbol](defn.RootPackage){(symbol, part) =>
				if '$' == part.last then
					symbol.declaredField(part.init)
				else
					symbol.declaredField(part)
			}
			.declaredType(nameSplit.last).head
	}

	extension (using Quotes)(term:quotes.reflect.Term) {
		/** `term.symbol.fieldMember` is empty if the symbol is a method invocation */
		def fieldMemberBaseClasses(name:String): quotes.reflect.Symbol = {
			import quotes.reflect.*
			term.tpe.baseClasses.foldLeft(Symbol.noSymbol)({(folding, clazz) =>
				if (folding.isNoSymbol) {
					clazz.fieldMember(name)
				} else {
					folding
				}
			})
		}

		def methodMemberBaseClasses(name:String): List[quotes.reflect.Symbol] = {
			term.tpe.baseClasses.flatMap(_.methodMember(name))
		}

		/** */
		def selectFieldMember(name:String):quotes.reflect.Term = {
			import quotes.reflect.*
			val fieldMember = term.fieldMemberBaseClasses(name)
			if (fieldMember.isNoSymbol) {
				report.errorAndAbort(s"${term.show} has no field `$name`")
			} else {
				term.select(fieldMember)
			}
		}

		def selectFieldMemberMaybeDynamic(name:String):quotes.reflect.Term = {
			import quotes.reflect._
			val symbol = term.symbol
			val fieldMember = term.fieldMemberBaseClasses(name)
			if ! fieldMember.isNoSymbol then
				term.select(fieldMember)

			else if symbol.typeRef <:< TypeRepr.of[scala.Dynamic] then
				val nameAsLiteral = Literal(StringConstant(name))
				symbol.methodMember("selectDynamic")
					.find({method =>
						method.paramSymss.lengthIs.==(1) &&
							method.paramSymss(0).lengthIs.==(1) &&
							method.paramSymss(0)(0).typeRef <:< TypeRepr.of[String]
					})
					.map(method => term.select(method)
							.appliedToArgs(nameAsLiteral :: Nil)
					)
					.getOrElse(
						report.errorAndAbort(s"${term.show} has no field `$name` and no `selectDynamic(String)` method")
					)

			else
				report.errorAndAbort(s"${term.show} has no field `$name` and is not Dynamic")
			end if
		}

		def selectAndApplyToArgsMaybeDynamicMaybeVarargs(name:String)(args:List[quotes.reflect.Term]):quotes.reflect.Term = {
			import quotes.reflect._
			val argTypes = args.map(_.tpe)

			def repeated(xs:List[quotes.reflect.Term]):quotes.reflect.Term = {
				val xsType = xs.map(_.tpe).reduceLeftOption({(_1, _2) => OrType(_1, _2)}).getOrElse(defn.NothingClass.typeRef)
				Typed(
					Repeated(xs, TypeTree.of(using xsType.asType)),
					Inferred(defn.RepeatedParamClass.typeRef.appliedTo(xsType))
				)
			}
			def tryCallStaticFixedargs(method:Symbol):Option[Term] = {
				val (typeSignature, termSignature) = {
					method.signature.paramSigs.partitionMap({
						case x:Int => Left(x)
						case x:String => Right(classSymbolThatDoesntResultInBadSymbolicReferenceWhenLookingForAnObjectContainedClass(x))
					})
				}
				// don't attempt methods with type arguments (yet?)
				if typeSignature.lengthIs.!=(0) then return None
				// Don't consider methods with multiple parameter lists
				if method.paramSymss.lengthIs.!=(typeSignature.length + 1) then return None

				// the method must be able to accept each argument
				if termSignature.lengthCompare(args) != 0 then return None
				if !termSignature.zip(argTypes).forall({(sig, arg) => arg <:< sig.typeRef}) then return None

				Option(term.select(method).appliedToArgs(args))
			}
			def tryCallStaticVarargs(method:Symbol):Option[Term] = {
				val (typeSignature, termSignature) = {
					method.signature.paramSigs.partitionMap({
						case x:Int => Left(x)
						case x:String => Right(classSymbolThatDoesntResultInBadSymbolicReferenceWhenLookingForAnObjectContainedClass(x))
					})
				}
				// don't attempt methods with type arguments (yet?)
				if typeSignature.lengthIs.!=(0) then return None
				// Don't consider methods with multiple parameter lists
				if method.paramSymss.lengthIs.!=(typeSignature.length + 1) then return None

				// varargs methods have at least the vararg term as a parameter
				if termSignature.lengthIs.<(1) then return None
				// if this is a vararg method, then the vararg term is the last term of the signature
				if ! (termSignature.last.typeRef =:= TypeRepr.of[Seq]) then return None
				// there must be enough arguments to fill the non-vararg method parameters
				if termSignature.tail.lengthCompare(argTypes) > 0 then return None

				val (fixedargs, varargs) = args.splitAt(termSignature.length - 1)
				if ! termSignature.zip(fixedargs).forall({(sig, arg) => arg.tpe <:< sig.typeRef}) then return None
				Option(term.select(method).appliedToArgs(fixedargs :+ repeated(varargs)))
			}
			def tryCallDynamic():Option[Term] = {
				val nameAsLiteral = Literal(StringConstant(name))

				// dont' attempt to do a dynamic call unless the class is dynamic
				if ! (term.symbol.typeRef <:< TypeRepr.of[scala.Dynamic]) then return None
				// dynamic only allows a class to have one `applyDynamic` method (unless type args, but we're ignoring those)
				val methods = term.symbol.methodMember("applyDynamic")
				if methods.lengthIs.!=(1) then return None
				val method = methods(0)

				val (typeSignature, termSignature) = {
					method.signature.paramSigs.partitionMap({
						case x:Int => Left(x)
						case x:String => Right(classSymbolThatDoesntResultInBadSymbolicReferenceWhenLookingForAnObjectContainedClass(x))
					})
				}
				// don't attempt methods with type arguments (yet?)
				if typeSignature.lengthIs.!=(0) then return None
				// check that the method has the expected applyDynamic signature - two param lists, first consisting of one string
				if method.paramSymss.lengthIs.!=(typeSignature.length + 2) then return None
				if method.paramSymss(typeSignature.length).lengthIs != 1 then return None
				if ! (termSignature.head.typeRef =:= TypeRepr.of[String]) then return None

				// first check if fixedargs will work
				if termSignature.tail.lengthCompare(args) == 0 &&
					termSignature.tail.zip(argTypes).forall({(sig, arg) => arg <:< sig.typeRef}) then
						return Option(term.select(method).appliedTo(nameAsLiteral).appliedToArgs(args))

				// then check if is a varargs method
				// varargs methods have at least the vararg term as a parameter
				if termSignature.tail.lengthIs.<(1) then return None
				// if this is a vararg method, then the vararg term is the last term of the signature
				if ! (termSignature.last.typeRef =:= TypeRepr.of[Seq]) then return None
				// there must be enough arguments to fill the non-vararg method parameters
				if termSignature.tail.tail.lengthCompare(argTypes) > 0 then return None
				val (fixedargs, varargs) = args.splitAt(termSignature.length - 2)
				if ! termSignature.tail.zip(fixedargs).forall({(sig, arg) => arg.tpe <:< sig.typeRef}) then return None
				Option(term.select(method).appliedTo(nameAsLiteral).appliedToArgs(fixedargs :+ repeated(varargs)))
			}


			val methodsWithName = term.methodMemberBaseClasses(name)

			methodsWithName.collectFirst(tryCallStaticFixedargs.unlift)
				.orElse(methodsWithName.collectFirst(tryCallStaticVarargs.unlift))
				.orElse(tryCallDynamic())
				.getOrElse(report.errorAndAbort(s"${term.show} has no method `$name` accepting (${argTypes.map(_.show).mkString(",")})"))
		}
	}
}
