package name.rayrobdod.stringContextParserCombinatorExample.uri

// the compiler creates smaller typecreator classes for `java.lang.String` than `scala.Predef.String`
import java.lang.String

import java.net.URI
import scala.reflect.macros.blackbox.Context
import name.rayrobdod.stringContextParserCombinator._
import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import name.rayrobdod.stringContextParserCombinatorExample.uri.ConcatenateStringImplicits._

object MacroImpl {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	def concatenateStrings(c:Context)(strings:Seq[c.Expr[String]]):c.Expr[String] = {
		import c.universe.Quasiquote
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
				strings.foreach(x => stats += q"$accumulatorExpr.append($x)")

				c.Expr[String](
					c.universe.Block(
						stats.toList,
						q"$accumulatorExpr.toString"
					)
				)
			}
		}
	}

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
			(variable <|> literal).repeat().map(xs => concatenateStrings(c)(xs))
		}


		/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
		val absolutePath:Interpolator[String] = (codePointIn("/") <~> (unreservedChar <|> escapedChar <|> codePointIn(":@&=+$,;/")).repeat())
		val absolutePathExpr:Interpolator[c.Expr[String]] = absolutePath.mapToExpr


		val fragmentOrQueryString:Interpolator[c.Expr[String]] = {
			val Arbitrary = (ofType[String] <|> uriChar.repeat(1).mapToExpr)
				.repeat()
				.map(xs => concatenateStrings(c)(xs))
			val Mapping = {
				implicit def AndThenElemElem:typeclass.Sequenced[c.Expr[String], c.Expr[String], List[c.Expr[String]]] = (a:c.Expr[String],b:c.Expr[String]) => a :: b :: Nil
				implicit def AndThenElemList:typeclass.Sequenced[c.Expr[String], List[c.Expr[String]], List[c.Expr[String]]] = (a:c.Expr[String], b:List[c.Expr[String]]) => a +: b
				implicit def AndThenListElem:typeclass.Sequenced[List[c.Expr[String]], c.Expr[String], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:c.Expr[String]) => a :+ b
				implicit def AndThenListList:typeclass.Sequenced[List[c.Expr[String]], List[c.Expr[String]], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:List[c.Expr[String]]) => a ++: b
				final class ListRepeatTypes[A] extends typeclass.Repeated[List[A], List[A]] {
					type Acc = scala.collection.mutable.Builder[A, List[A]]
					def init():Acc = List.newBuilder
					def append(acc:Acc, elem:List[A]):Acc = {acc ++= elem}
					def result(acc:Acc):List[A] = acc.result()
				}
				implicit def ListRepeatTypes[A]:typeclass.Repeated[List[A], List[A]] = new ListRepeatTypes[A]
				val EqualsChar = codePointIn("=").map(_.toString).mapToExpr
				val AndChar = codePointIn("&").map(_.toString).mapToExpr

				val tupleConcatFun = q""" {ab:(String, String) => ab._1 + "=" + ab._2} """
				val lit:Interpolator[c.Expr[String]] = (escapedChar <|> unreservedChar <|> codePointIn(";?:@+$,")).repeat().mapToExpr
				val str:Interpolator[c.Expr[String]] = ofType[String]
				val str2:Interpolator[c.Expr[String]] = str <|> lit
				val pair:Interpolator[List[c.Expr[String]]] = ofType(c.typeTag[scala.Tuple2[String, String]])
					.map(x => List(
						c.Expr[String](q"$x._1"),
						constExpr("="),
						c.Expr[String](q"$x._2")
					))
				val pair2:Interpolator[List[c.Expr[String]]] = pair <|> (str2 <~> EqualsChar <~> str2)
				val map:Interpolator[List[c.Expr[String]]] = ofType(c.typeTag[scala.collection.Map[String, String]])
					.map(x => c.Expr[List[String]](q"$x.map($tupleConcatFun)"))
					.map(x => List(c.Expr[String](q""" $x.mkString("&") """)))
				val mapOrPair:Interpolator[List[c.Expr[String]]] = map <|> pair2

				(mapOrPair <~> (AndChar <~> mapOrPair).repeat())
					.map(xs => concatenateStrings(c)(xs))
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
