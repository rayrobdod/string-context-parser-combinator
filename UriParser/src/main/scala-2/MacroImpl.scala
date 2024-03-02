package name.rayrobdod.stringContextParserCombinatorExample.uri

// the compiler creates smaller typecreator classes for `java.lang.String` than `scala.Predef.String`
import java.lang.String

import java.net.URI
import scala.reflect.macros.blackbox.Context
import name.rayrobdod.stringContextParserCombinator._
import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import name.rayrobdod.stringContextParserCombinatorExample.uri.ConcatenateStringImplicits._

object MacroImpl {
	def stringContext_uri(c:Context {type PrefixType = UriStringContext})(args:c.Expr[Any]*):c.Expr[URI] = {
		val LeafParsers = Interpolator.contextInterpolators(c)
		import LeafParsers._
		import c.universe.Quasiquote

		implicit val thisCToExpr = typeclass.ToExprMapping.forContext(c)
		val constExpr:Function1[String, c.Expr[String]] = {x => c.Expr(c.universe.Literal(c.universe.Constant(x)))}
		val constNullExpr:c.Expr[Null] = c.Expr(c.universe.Literal(c.universe.Constant(null)))
		val constNegOneExpr:c.Expr[Int] = c.Expr(c.universe.Literal(c.universe.Constant(-1)))
		def parseByteHex(x:(Char, Char)):Int = java.lang.Integer.parseInt(s"${x._1}${x._2}", 16)

		val hexChar:Interpolator[Char] = charWhere(c => '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F').opaque("hexChar")

		val alphaChar:Interpolator[CodePoint] = codePointWhere(c => 'a' <= c.intValue && c.intValue <= 'z' || 'A' <= c.intValue && c.intValue <= 'Z').opaque("alphaChar")
		val digitChar:Interpolator[CodePoint] = codePointWhere(c => '0' <= c.intValue && c.intValue <= '9').opaque("digitChar")
		val alphaNumChar:Interpolator[CodePoint] = alphaChar <|> digitChar
		val unreservedChar:Interpolator[CodePoint] = alphaNumChar <|> codePointIn("-_.!~*'()")

		val escapedChar:Interpolator[CodePoint] = {
			implicit val Utf8ContinuationAndThen:typeclass.Sequenced[Int, Int, Int] = {(a:Int, b:Int) => a << 6 | b}
			val escapedContinuation:Interpolator[Int] = (isString("%") <~> charIn("89ABab") <~> hexChar).map({x => (parseByteHex(x) & 0x3F)})

			(isString("%") <~> (
				(charIn("01234567") <~> hexChar).map({x => CodePoint.unsafe_apply(parseByteHex(x))}) <|>
				(charIn("cdCD") <~> hexChar).map({x => (parseByteHex(x) & 0x1F)}).<~>(escapedContinuation).map(CodePoint.unsafe_apply _) <|>
				(charIn("eE") <~> hexChar).map({x => (parseByteHex(x) & 0x0F)}).<~>(escapedContinuation).<~>(escapedContinuation).map(CodePoint.unsafe_apply _) <|>
				(charIn("fF") <~> charIn("01234567")).map({x => (parseByteHex(x) & 0x07)}).<~>(escapedContinuation).<~>(escapedContinuation).<~>(escapedContinuation).map(CodePoint.unsafe_apply _)
			))
		}

		val uriNoSlashChar:Interpolator[CodePoint] = escapedChar <|> unreservedChar <|> codePointIn(";?:@&=+$,")
		val uriChar:Interpolator[CodePoint] = uriNoSlashChar <|> codePointIn("/")

		val scheme:Interpolator[c.Expr[String]] = {
			val literal:Interpolator[c.Expr[String]] = (alphaChar <~> (alphaNumChar <|> codePointIn("+-.")).repeat()).mapToExpr
			literal
		}

		val userInfo:Interpolator[c.Expr[String]] = {
			val literal:Interpolator[c.Expr[String]] = (unreservedChar <|> escapedChar <|> codePointIn(";:&=+$,")).repeat().mapToExpr
			literal
		}

		val host:Interpolator[c.Expr[String]] = {
			val label:Interpolator[String] = alphaNumChar <~> ((alphaNumChar <|> codePointIn("-")).repeat(strategy = Greedy) <~> alphaNumChar).optionally()
			val topLabel:Interpolator[String] = alphaChar <~> ((alphaNumChar <|> codePointIn("-")).repeat(strategy = Greedy) <~> alphaNumChar).optionally()
			val literalName:Interpolator[c.Expr[String]] = ((label <~> codePointIn(".")).attempt.repeat() <~> topLabel).mapToExpr.opaque("HostName")
			val literalIpv4:Interpolator[c.Expr[String]] = {
				val segment:Interpolator[String] = (
					isString("0").map(_ => "0") <|>
						(codePointIn("1") <~> digitChar.repeat(0,2)) <|>
						(codePointIn("2") <~> (
							(codePointIn("01234") <~> digitChar.optionally()) <|>
							(codePointIn("5") <~> codePointIn("012345").optionally()) <|>
							(codePointIn("6789").map(_.toString))
						).optionally()) <|>
						(codePointIn("3456789") <~> digitChar.optionally())
				)
				(segment <~> (codePointIn(".") <~> segment).repeat(3,3)).mapToExpr.opaque("IPv4 Address")
			}
			val literalIpv6:Interpolator[c.Expr[String]] = {
				val segment:Interpolator[String] = hexChar.repeat(1,4)
				val colonSegment:Interpolator[String] = codePointIn(":") <~> segment
				val segmentColon:Interpolator[String] = segment <~> codePointIn(":")

				val value:Interpolator[String] = codePointIn("[") <~> (
					(codePointIn(":") <~> (colonSegment.repeat(1, 7).attempt <|> codePointIn(":").map(_.toString))) <|>
					(segmentColon <~> (
						(colonSegment <~> colonSegment.repeat(0, 6)) <|>
						(segmentColon <~> (
							(colonSegment <~> colonSegment.repeat(0, 5)) <|>
							(segmentColon <~> (
								(colonSegment <~> colonSegment.repeat(0, 4)) <|>
								(segmentColon <~> (
									(colonSegment <~> colonSegment.repeat(0, 3)) <|>
									(segmentColon <~> (
										(colonSegment <~> colonSegment.repeat(0, 2)) <|>
										(segmentColon <~> (
											(colonSegment <~> colonSegment.repeat(0, 1)) <|>
											(segment <~> colonSegment)
										))
									))
								))
							))
						))
					))
				) <~> codePointIn("]")
				value.mapToExpr.opaque("IPv6 Address")
			}
			/* Luckily, the URI constructor seems to be able to surround v6 addresses in brackets automatically, so that we don't have to */
			val variableInetAddress:Interpolator[c.Expr[String]] = ofType(c.typeTag[java.net.InetAddress])
				.map(x => c.Expr(q"$x.getHostName()"))
			variableInetAddress <|> literalIpv4 <|> literalIpv6 <|> literalName
		}

		val port:Interpolator[c.Expr[Int]] = {
			val literal:Interpolator[c.Expr[Int]] = digitChar.repeat(1)
				.map({x => java.lang.Integer.parseInt(x)})
				.map({x => c.Expr(c.universe.Literal(c.universe.Constant(x)))})
				.opaque("Port")
			val literalEmpty:Interpolator[c.Expr[Int]] = isString("").map({_ => constNegOneExpr})
			val variable:Interpolator[c.Expr[Int]] = ofType[Int]
			variable <|> literal <|> literalEmpty
		}

		val hostPort:Interpolator[(c.Expr[String], c.Expr[Int])] = {
			val literal = host <~> (isString(":") <~> port)
				.optionally().map(_.getOrElse(c.Expr(q"-1")))
			val SockAddr = ofType(c.typeTag[java.net.InetSocketAddress])
				.map(x => (
					c.Expr(q"$x.getHostString()"),
					c.Expr(q"$x.getPort()")
				))
			SockAddr <|> literal
		}
		val server:Interpolator[(c.Expr[String], (c.Expr[String], c.Expr[Int]))] =
			(userInfo <~> isString("@")).attempt.optionally().map(_.getOrElse(constNullExpr)) <~> hostPort

		val opaquePart:Interpolator[c.Expr[String]] = {
			val variable:Interpolator[c.Expr[String]] = ofType[String]
			val literal:Interpolator[c.Expr[String]] = (uriNoSlashChar <~> uriChar.repeat()).mapToExpr
			(variable <|> literal).repeat()(typeclass.Repeated.forContextConcatenateString(c))
		}


		/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
		val absolutePath:Interpolator[String] = (codePointIn("/") <~> (unreservedChar <|> escapedChar <|> codePointIn(":@&=+$,;/")).repeat())
		val absolutePathExpr:Interpolator[c.Expr[String]] = absolutePath.mapToExpr


		val fragmentOrQueryString:Interpolator[c.Expr[String]] = {
			val Arbitrary = (ofType[String] <|> uriChar.repeat(1).mapToExpr)
				.repeat()(typeclass.Repeated.forContextConcatenateString(c))
			val Mapping = {
				val accumulatorName = c.freshName(c.universe.TermName("accumulator$"))
				val accumulatorTypeTree = c.universe.TypeTree(
					c.universe.rootMirror.staticClass("scala.collection.mutable.StringBuilder").asType.toTypeConstructor
				)
				val accumulatorIdent = c.universe.Ident(accumulatorName)
				val accumulatorValDef = c.universe.ValDef(
					c.universe.NoMods,
					accumulatorName,
					accumulatorTypeTree,
					q"new $accumulatorTypeTree()",
				)

				class StringExpr private (val isEmpty: Boolean, private val direct: Option[c.Expr[String]], private val accStats: List[c.Tree]) {
					def ++(other: StringExpr): StringExpr = {
						if (this.isEmpty) {
							other
						} else if (other.isEmpty) {
							this
						} else {
							new StringExpr(false, None, this.accStats ++: other.accStats)
						}
					}
					def result: c.Expr[String] = {
						this.direct match {
							case Some(x) => x
							case None => {
								c.Expr[String](
									c.universe.Block(
										accumulatorValDef :: accStats,
										q"$accumulatorIdent.toString"
									)
								)
							}
						}
					}
				}
				object StringExpr {
					def empty: StringExpr = new StringExpr(true, Option(constExpr("")), Nil)
					def single(direct: c.Expr[String]): StringExpr = new StringExpr(false, Some(direct), List(q"""$accumulatorIdent.append($direct)"""))
					def single(direct: c.Expr[String], accStats: List[c.Tree]): StringExpr = new StringExpr(false, Some(direct), accStats)
					def multiple(accStats: List[c.Tree]): StringExpr = new StringExpr(false, None, accStats)
				}

				implicit def AndThenStringExpr: typeclass.Sequenced[StringExpr, StringExpr, StringExpr] = (a:StringExpr, b:StringExpr) => a ++ b
				final class RepeatStringExpr extends typeclass.Repeated[StringExpr, c.Expr[String]] {
					type Acc = StringExpr
					def init():Acc = StringExpr.empty
					def append(acc:Acc, elem:StringExpr):Acc = {
						if (acc.isEmpty) {
							elem
						} else {
							acc ++ StringExpr.single(constExpr("&")) ++ elem
						}
					}
					def result(acc:Acc):c.Expr[String] = acc.result
				}
				implicit def RepeatStringExpr: typeclass.Repeated[StringExpr, c.Expr[String]] = new RepeatStringExpr

				val EqualsChar = isString("=").map(_ => StringExpr.single(constExpr("=")))
				val AndChar = isString("&")

				val tupleConcatFun = q""" {pair:(String, String) => pair._1 + "=" + pair._2} """

				val literalString:Interpolator[c.Expr[String]] = (escapedChar <|> unreservedChar <|> codePointIn(";?:@+$,")).repeat(1).mapToExpr
				val holeString:Interpolator[c.Expr[String]] = ofType[String]
				val string:Interpolator[StringExpr] = (holeString <|> literalString).map(s => StringExpr.single(s))

				val holePair:Interpolator[StringExpr] = ofType(c.typeTag[scala.Tuple2[String, String]])
					.map(x =>
						StringExpr.multiple(
							List(
								q"$accumulatorIdent.append($x._1)",
								q"""$accumulatorIdent.append("=")""",
								q"$accumulatorIdent.append($x._2)",
							)
						)
					)
				val literalPair: Interpolator[StringExpr] = (string <~> EqualsChar <~> string)
				val pair:Interpolator[StringExpr] = holePair <|> literalPair

				val map:Interpolator[StringExpr] = ofType(c.typeTag[scala.collection.Map[String, String]])
					.map(m => StringExpr.single(
						c.Expr[String](q"""$m.map($tupleConcatFun).mkString("&")"""),
						List(q"""$m.map($tupleConcatFun).addString($accumulatorIdent, "&")"""),
					))

				val mapOrPair:Interpolator[StringExpr] = map <|> pair

				mapOrPair.repeat(min = 1, delimiter = AndChar)(using RepeatStringExpr)
			}
			Mapping.attempt <|> Arbitrary
		}
		val query:Interpolator[c.Expr[String]] = (isString("?") <~> fragmentOrQueryString).optionally().map(_.getOrElse(constNullExpr))
		val fragment:Interpolator[c.Expr[String]] = (isString("#") <~> fragmentOrQueryString).optionally().map(_.getOrElse(constNullExpr))


		val relativePath:Interpolator[String] =
			(escapedChar <|> unreservedChar <|> codePointIn(";@&=+$,")).repeat(1) <~> absolutePath.optionally()
		val netPath:Interpolator[((c.Expr[String], (c.Expr[String], c.Expr[Int])), c.Expr[String])] = isString("//") <~> server <~> absolutePathExpr
		val noServer:(c.Expr[String], (c.Expr[String], c.Expr[Int])) = (constNullExpr, (constNullExpr, constNegOneExpr))

		val absoluteUri:Interpolator[c.Expr[URI]] = {
			scheme <~>
			isString(":") flatMap
			({scheme:c.Expr[String] =>
				(isString("//") <~>
					server <~>
					absolutePathExpr.optionally().map(_.getOrElse(constNullExpr)) <~>
					query <~>
					fragment
				).map({case ((((user, (host, port)), path), query), fragment) =>
					c.Expr(q"""
						new java.net.URI(
							$scheme,
							$user,
							$host,
							$port,
							$path,
							$query,
							$fragment
						)
					""")
				}) <|>
				(opaquePart <~> fragment).map({case (ssp, frag) =>
					c.Expr(q"""
						new java.net.URI(
							$scheme,
							$ssp,
							$frag
						)
					""")
				})
			})
		}

		val relativeUri:Interpolator[c.Expr[URI]] = {
			((netPath.attempt
				<|> absolutePathExpr.map(x => (noServer, x)).attempt
				<|> relativePath.map(x => (noServer, constExpr(x)))
				<~> query
				<~> fragment
				).map({case ((((user, (host, port)), path), query), fragment) =>
					c.Expr(q"""
						new java.net.URI(
							$constNullExpr,
							$user,
							$host,
							$port,
							$path,
							$query,
							$fragment
						)
					""")
				})
			)
		}

		val resolvedUri:Interpolator[c.Expr[URI]] = {
			(ofType[URI] <~> relativeUri).map({params =>
				val (base, resolvant) = params
				c.Expr(q"$base.resolve($resolvant)")
			})
		}

		val uri:Interpolator[c.Expr[URI]] = (resolvedUri.attempt <|> absoluteUri.attempt <|> relativeUri) <~> end

		val extensionClassName = "name.rayrobdod.stringContextParserCombinatorExample.uri.package.UriStringContext"
		uri.interpolate(c)(extensionClassName)(args.toList)
	}
}
