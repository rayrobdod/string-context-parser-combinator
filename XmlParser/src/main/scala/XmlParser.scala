package name.rayrobdod.stringContextParserCombinatorExample.xml

import scala.collection.immutable.BitSet
import scala.collection.immutable.{Map, Seq}
import scala.quoted.*
import name.rayrobdod.stringContextParserCombinator.{Interpolator => _, _}
import name.rayrobdod.stringContextParserCombinator.Interpolator.*

private[xml] object XmlParser {
	extension [A] (p:Interpolator[A])
		def collect[Z](pf: PartialFunction[A, Z], msg: String): Interpolator[Z] =
			p.filter(pf.isDefinedAt _, msg).map(pf.apply _)


	/** Represents a name with a specified short namespace, but an unknown namespace */
	final case class BoundName(prefix:Option[String], local:String)

	final case class NamespaceBindingOne(prefix:Option[String], namespace:String)
	type NamespaceBinding = Map[Option[String], String]

	final case class Attribute(key:BoundName, value:List[Expr[Any]])


	val xmlNameFirstChar:BitSet = {
		import scala.Predef.intWrapper
		val pattern = java.util.regex.Pattern.compile("""[A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\x{10000}-\x{EFFFF}]""")
		val builder = BitSet.newBuilder
		(0 until 0x110000)
			.filter(x => pattern.matcher(new String(Array[Int](x), 0, 1)).matches)
			.foreach(builder.+= _)
		builder.result
	}
	val xmlNameRestChar:BitSet = {
		import scala.Predef.intWrapper
		val pattern = java.util.regex.Pattern.compile("""[\-\.0-9\u00B7\u0300-\u036F\u203F-\u2040]""")
		val builder = BitSet.newBuilder
		(0 until 0x110000)
			.filter(x => pattern.matcher(new String(Array[Int](x), 0, 1)).matches)
			.foreach(builder.+= _)
		builder.result
	}
	val xmlAllowedChar:BitSet = {
		import scala.Predef.intWrapper
		val bits = Seq(0x9, 0xA, 0xD) ++ (0x20 to 0xD7FF) ++ (0xE000 to 0xFFFD) ++ (0x10000 to 0x10FFFF)
		BitSet(bits:_*)
	}

	private val whitespace:Interpolator[Unit] = charIn("\n\r\t ").map(_ => ())

	/* @see https://www.w3.org/TR/xml/#sec-line-ends
	 * 2.11: the XML processor MUST behave as if it normalized all line breaks in external parsed entities
	 */
	private val lineBreak:Interpolator[CodePoint] = ((isString("\r\n") orElse isString("\r")) orElse isString("\n")).map(_ => CodePoint('\n'))

	private def charRef(using Quotes):Interpolator[CodePoint] = {
		def hexInteger = charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repeat(1).map(Integer.parseInt(_, 16))
		def decInteger = charIn('0' to '9').repeat(1).map(Integer.parseInt(_, 10))

		isString("&#") andThen (((isString("x") andThen hexInteger) orElse decInteger) andThen isString(";"))
			.filter(xmlAllowedChar, "")
			.collect({(c:Int) => CodePoint(c)}.unlift, "")
			.opaque("valid codepoint")
	}

	given typeclass.Sequenced[CodePoint, String, String] = {(head:CodePoint, tail:String) => s"${head}${tail}"}

	private val xmlNameNoColon:Interpolator[String] = {
		val firstChar = codePointWhere(x => xmlNameFirstChar.contains(x.intValue))
		val restChar = firstChar orElse codePointWhere(x => xmlNameRestChar.contains(x.intValue))
		(firstChar andThen restChar.repeat())
	}
	private val xmlName:Interpolator[String] = {
		val firstChar = codePointIn(":") orElse codePointWhere(x => xmlNameFirstChar.contains(x.intValue))
		val restChar = firstChar orElse codePointWhere(x => xmlNameRestChar.contains(x.intValue))
		(firstChar andThen restChar.repeat())
	}
	private def xmlAllowedChar(exclude:String):Interpolator[CodePoint] = {
		val exclude2:Set[Int] = Set(exclude.codePoints.toArray:_*)
		if exclude2.contains('\n') then
			val bits = ((xmlAllowedChar -- exclude2) - '\r')
			codePointWhere(x => bits.contains(x.intValue))
				.opaque("xml char excluding " + exclude2.mkString("[", ",", "]"))
		else
			val bits = ((xmlAllowedChar -- exclude2) - '\r' - '\n')
			(lineBreak orElse codePointWhere(x => bits.contains(x.intValue)))
				.opaque("xml char excluding " + exclude2.mkString("[", ",", "]"))

	}


	private val boundName:Interpolator[BoundName] = {
		(((xmlNameNoColon andThen isString(":")).attempt.optionally()) andThen xmlName)
			.map(x => BoundName(x._1, x._2))
	}

	private val namespaceBinding:Interpolator[NamespaceBindingOne] = {
		val namespaceValue:Interpolator[String] = {
			((isString("\"") andThen xmlAllowedChar("\"").repeat() andThen isString("\"")) orElse
			(isString("\'") andThen xmlAllowedChar("\'").repeat() andThen isString("\'")))
				.map(x => x.mkString)
		}

		isString("xmlns") andThen (
			(isString("=") andThen namespaceValue).map(x => NamespaceBindingOne(None, x)) orElse
			(isString(":") andThen xmlName andThen isString("=") andThen namespaceValue).map(x => NamespaceBindingOne(Option(x._1), x._2))
		)
	}

	private def attribute(factory:Expr[XmlFactory])(using Quotes):Interpolator[Attribute] = {
		def valueText(excluding:Char) = (charRef orElse xmlAllowedChar("<&" + excluding)).repeat(1)
			.map({data =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("values")
					.selectFieldMemberMaybeDynamic(data)
			})
		def value(excluding:Char) = (entity(factory) orElse valueText(excluding)).repeat(1)

		(
			boundName andThen isString("=") andThen (
					(isString("\"\"") orElse isString("\'\'")).map(_ => Nil) orElse
					(isString("\"") andThen value('\"') andThen isString("\"")) orElse
					(isString("\'") andThen value('\'') andThen isString("\'"))
				)
		)
			.map((k,v) => Attribute(k, v.map(_.asExpr)))
	}

	private def cdata(factory:Expr[XmlFactory])(using Quotes):Interpolator[quotes.reflect.Term] = {
		given typeclass.Eithered[CodePoint, String, String] with
			def left(elem:CodePoint):String = elem.toString
			def right(elem:String):String = elem
		given typeclass.Sequenced[CodePoint, CodePoint, String] = {(head:CodePoint, tail:CodePoint) => s"${head}${tail}"}
		given typeclass.Sequenced[String, CodePoint, String] = {(head:String, tail:CodePoint) => s"${head}${tail}"}

		(
			isString("<![CDATA[")
			andThen (
				(xmlAllowedChar("]")) orElse
				(codePointIn("]") andThen xmlAllowedChar("]")).attempt orElse
				(codePointIn("]") andThen codePointIn("]") andThen xmlAllowedChar(">")).attempt
			).repeat().map(_.mkString)
			andThen isString("]]>")
		)
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("cdata")
					.selectFieldMemberMaybeDynamic(name)
			})
	}

	private def entity(factory:Expr[XmlFactory])(using Quotes):Interpolator[quotes.reflect.Term] = {
		(isString("&") andThen xmlName andThen isString(";"))
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("entities")
					.selectFieldMemberMaybeDynamic(name)
			})
			.attempt
	}

	private def text(factory:Expr[XmlFactory])(using Quotes):Interpolator[quotes.reflect.Term] = {
		(charRef orElse xmlAllowedChar("<&")).repeat(1)
			.map({data =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("texts")
					.selectFieldMemberMaybeDynamic(data)
			})
	}

	private def comment(factory:Expr[XmlFactory])(using Quotes):Interpolator[quotes.reflect.Term] = {
		(
			isString("<!--")
			andThen (
				(xmlAllowedChar("-")) orElse
				(codePointIn("-") andThen xmlAllowedChar("-")).attempt
			).repeat().map(_.mkString)
			andThen isString("-->")
		)
			.map({name =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("comments")
					.selectFieldMemberMaybeDynamic(name)
			})
	}

	private def processingInstruction(factory:Expr[XmlFactory])(using Quotes):Interpolator[quotes.reflect.Term] = {
		(
			isString("<?")
			andThen xmlName.filter(_ != "xml", "not `xml`")
			andThen whitespace
			andThen (
				(xmlAllowedChar("?")) orElse
				(codePointIn("?") andThen xmlAllowedChar(">")).attempt
			).repeat().map(_.mkString)
			andThen isString("?>")
		)
			.map({(target, value) =>
				import quotes.reflect._
				factory.asTerm
					.selectFieldMember("processInstructions")
					.selectAndApplyToArgsMaybeDynamicMaybeVarargs(target)(List(Literal(StringConstant(value))))
			})
	}

	private def fragment(factory:Expr[XmlFactory], nsb: NamespaceBinding)(using Quotes):Interpolator[List[quotes.reflect.Term]] = {
		(
			entity(factory) orElse
			text(factory) orElse
			cdata(factory) orElse
			comment(factory) orElse
			processingInstruction(factory) orElse
			elem(factory, nsb).attempt
		).repeat()
	}

	private def elem(factory:Expr[XmlFactory], nsb:NamespaceBinding)(using Quotes):Interpolator[quotes.reflect.Term] = {
		(
			isString("<") andThen
				boundName andThen
				(whitespace.repeat(1) andThen (
					namespaceBinding.map(Left.apply _) orElse
					attribute(factory).map(Right.apply _)
				)).attempt.repeat() andThen
				whitespace.repeat()
			)
			.map({parts =>
				val (name, nsAndAttrs:List[Either[NamespaceBindingOne, Attribute]]) = parts
				val nsb2:NamespaceBinding = nsb ++ nsAndAttrs.collect({case Left(x) => (x._1, x._2)})
				val attrs = nsAndAttrs.collect({case Right(x) =>
					import quotes.reflect._
					val Attribute(BoundName(ns, localName), value2) = x
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
				})

				(name, attrs, nsb2)
			})
			.flatMap({(parts) =>
				val (elemName, attributes, nsb2) = parts

				(isString("/>").map(_ => List.empty) orElse (
					(isString(">") andThen
					(fragment(factory, nsb2) andThen
					isString("</"))) andThen
					boundName.filter(x => x == elemName, s"$elemName").map(_ => ()) andThen
					whitespace.repeat() andThen
					isString(">")
				))
					.map(children => ((elemName, attributes, nsb2, children)))
			})
			.map({parts =>
				val (elemName, attributes, nsb2, children) = parts
				import quotes.reflect._
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
					.selectAndApplyToArgsMaybeDynamicMaybeVarargs(elemName.local)(attributes ++ children)
			})
	}


	def interpolate(sc:Expr[scala.StringContext], args:Expr[Seq[Any]], factory:Expr[XmlFactory])(using Quotes):Expr[Any] = {
		import quotes.reflect.*
		val (factoryExpr, factoryType) = factory match {case '{ $x: t } => ((x, Type.of[t])) }

		val initialNsb:NamespaceBinding = Map(
			(None, ""),
			(Some("xml"), "http://www.w3.org/XML/1998/namespace"),
			(Some("xmlns"), "http://www.w3.org/200/xmlns/")
		)

		factoryExpr.asTerm
			.selectAndApplyToArgsMaybeDynamicMaybeVarargs
				("literal")
				((fragment(factoryExpr, initialNsb) andThen end).interpolate(sc, args))
			.asExpr
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
