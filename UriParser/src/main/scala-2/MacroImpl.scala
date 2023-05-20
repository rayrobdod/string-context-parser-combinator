package com.rayrobdod.stringContextParserCombinatorExample.uri

// the compiler creates smaller typecreator classes for `java.lang.String` than `scala.Predef.String`
import java.lang.String

import java.net.URI
import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import com.rayrobdod.stringContextParserCombinatorExample.uri.ConcatenateStringImplicits._

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

		val alphaChar:Interpolator[CodePoint] = codePointWhere(c => 'a' <= c.value && c.value <= 'z' || 'A' <= c.value && c.value <= 'Z').opaque("alphaChar")
		val digitChar:Interpolator[CodePoint] = codePointWhere(c => '0' <= c.value && c.value <= '9').opaque("digitChar")
		val alphaNumChar:Interpolator[CodePoint] = alphaChar orElse digitChar
		val unreservedChar:Interpolator[CodePoint] = alphaNumChar orElse codePointIn("-_.!~*'()")

		val escapedChar:Interpolator[CodePoint] = {
			implicit object Utf8ContinuationAndThen extends typeclass.Sequenced[Int, Int, Int] {def aggregate(a:Int, b:Int):Int = a << 6 | b}
			val escapedContinuation:Interpolator[Int] = (isString("%") andThen charIn("89ABab") andThen hexChar).map({x => (parseByteHex(x) & 0x3F)})

			(isString("%") andThen (
				(charIn("01234567") andThen hexChar).map({x => CodePoint(parseByteHex(x))}) orElse
				(charIn("cdCD") andThen hexChar).map({x => (parseByteHex(x) & 0x1F)}).andThen(escapedContinuation).map(CodePoint.apply _) orElse
				(charIn("eE") andThen hexChar).map({x => (parseByteHex(x) & 0x0F)}).andThen(escapedContinuation).andThen(escapedContinuation).map(CodePoint.apply _) orElse
				(charIn("fF") andThen charIn("01234567")).map({x => (parseByteHex(x) & 0x07)}).andThen(escapedContinuation).andThen(escapedContinuation).andThen(escapedContinuation).map(CodePoint.apply _)
			))
		}

		val uriNoSlashChar:Interpolator[CodePoint] = escapedChar orElse unreservedChar orElse codePointIn(";?:@&=+$,")
		val uriChar:Interpolator[CodePoint] = uriNoSlashChar orElse codePointIn("/")

		val scheme:Interpolator[c.Expr[String]] = {
			val literal:Interpolator[c.Expr[String]] = (alphaChar andThen (alphaNumChar orElse codePointIn("+-.")).repeat()).mapToExpr
			literal
		}

		val userInfo:Interpolator[c.Expr[String]] = {
			val literal:Interpolator[c.Expr[String]] = (unreservedChar orElse escapedChar orElse codePointIn(";:&=+$,")).repeat().mapToExpr
			literal
		}

		val host:Interpolator[c.Expr[String]] = {
			val label:Interpolator[String] = alphaNumChar andThen ((alphaNumChar orElse codePointIn("-")).repeat(strategy = Greedy) andThen alphaNumChar).optionally()
			val topLabel:Interpolator[String] = alphaChar andThen ((alphaNumChar orElse codePointIn("-")).repeat(strategy = Greedy) andThen alphaNumChar).optionally()
			val literalName:Interpolator[c.Expr[String]] = ((label andThen codePointIn(".")).attempt.repeat() andThen topLabel).mapToExpr.opaque("HostName")
			val literalIpv4:Interpolator[c.Expr[String]] = {
				val segment:Interpolator[String] = (
					isString("0").map(_ => "0") orElse
						(codePointIn("1") andThen digitChar.repeat(0,2)) orElse
						(codePointIn("2") andThen (
							(codePointIn("01234") andThen digitChar.optionally()) orElse
							(codePointIn("5") andThen codePointIn("012345").optionally()) orElse
							(codePointIn("6789").map(_.toString))
						).optionally()) orElse
						(codePointIn("3456789") andThen digitChar.optionally())
				)
				(segment andThen (codePointIn(".") andThen segment).repeat(3,3)).mapToExpr.opaque("IPv4 Address")
			}
			val literalIpv6:Interpolator[c.Expr[String]] = {
				val segment:Interpolator[String] = hexChar.repeat(1,4)
				val colonSegment:Interpolator[String] = codePointIn(":") andThen segment
				val segmentColon:Interpolator[String] = segment andThen codePointIn(":")

				val value:Interpolator[String] = codePointIn("[") andThen (
					(codePointIn(":") andThen (colonSegment.repeat(1, 7).attempt orElse codePointIn(":").map(_.toString))) orElse
					(segmentColon andThen (
						(colonSegment andThen colonSegment.repeat(0, 6)) orElse
						(segmentColon andThen (
							(colonSegment andThen colonSegment.repeat(0, 5)) orElse
							(segmentColon andThen (
								(colonSegment andThen colonSegment.repeat(0, 4)) orElse
								(segmentColon andThen (
									(colonSegment andThen colonSegment.repeat(0, 3)) orElse
									(segmentColon andThen (
										(colonSegment andThen colonSegment.repeat(0, 2)) orElse
										(segmentColon andThen (
											(colonSegment andThen colonSegment.repeat(0, 1)) orElse
											(segment andThen colonSegment)
										))
									))
								))
							))
						))
					))
				) andThen codePointIn("]")
				value.mapToExpr.opaque("IPv6 Address")
			}
			/* Luckily, the URI constructor seems to be able to surround v6 addresses in brackets automatically, so that we don't have to */
			val variableInetAddress:Interpolator[c.Expr[String]] = ofType(c.typeTag[java.net.InetAddress])
				.map(x => c.Expr(q"$x.getHostName()"))
			variableInetAddress orElse literalIpv4 orElse literalIpv6 orElse literalName
		}

		val port:Interpolator[c.Expr[Int]] = {
			val literal:Interpolator[c.Expr[Int]] = digitChar.repeat(1)
				.map({x => java.lang.Integer.parseInt(x)})
				.map({x => c.Expr(c.universe.Literal(c.universe.Constant(x)))})
				.opaque("Port")
			val literalEmpty:Interpolator[c.Expr[Int]] = isString("").map({_ => constNegOneExpr})
			val variable:Interpolator[c.Expr[Int]] = ofType[Int]
			variable orElse literal orElse literalEmpty
		}

		val hostPort:Interpolator[(c.Expr[String], c.Expr[Int])] = {
			val literal = host andThen (isString(":") andThen port)
				.optionally().map(_.getOrElse(c.Expr(q"-1")))
			val SockAddr = ofType(c.typeTag[java.net.InetSocketAddress])
				.map(x => (
					c.Expr(q"$x.getHostString()"),
					c.Expr(q"$x.getPort()")
				))
			SockAddr orElse literal
		}
		val server:Interpolator[(c.Expr[String], (c.Expr[String], c.Expr[Int]))] =
			(userInfo andThen isString("@")).attempt.optionally().map(_.getOrElse(constNullExpr)) andThen hostPort

		val opaquePart:Interpolator[c.Expr[String]] = {
			val variable:Interpolator[c.Expr[String]] = ofType[String]
			val literal:Interpolator[c.Expr[String]] = (uriNoSlashChar andThen uriChar.repeat()).mapToExpr
			(variable orElse literal).repeat().map(xs => concatenateStrings(c)(xs))
		}


		/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
		val absolutePath:Interpolator[String] = (codePointIn("/") andThen (unreservedChar orElse escapedChar orElse codePointIn(":@&=+$,;/")).repeat())
		val absolutePathExpr:Interpolator[c.Expr[String]] = absolutePath.mapToExpr


		val fragmentOrQueryString:Interpolator[c.Expr[String]] = {
			val Arbitrary = (ofType[String] orElse uriChar.repeat(1).mapToExpr)
				.repeat()
				.map(xs => concatenateStrings(c)(xs))
			val Mapping = {
				import scala.language.implicitConversions
				implicit def fn2then[A,B,Z](fn:(A,B) => Z):typeclass.Sequenced[A,B,Z] = new typeclass.Sequenced[A,B,Z]{
					def aggregate(a:A, b:B):Z = fn(a,b)
				}
				implicit def AndThenElemElem:typeclass.Sequenced[c.Expr[String], c.Expr[String], List[c.Expr[String]]] = (a:c.Expr[String],b:c.Expr[String]) => a :: b :: Nil
				implicit def AndThenElemList:typeclass.Sequenced[c.Expr[String], List[c.Expr[String]], List[c.Expr[String]]] = (a:c.Expr[String], b:List[c.Expr[String]]) => a +: b
				implicit def AndThenListElem:typeclass.Sequenced[List[c.Expr[String]], c.Expr[String], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:c.Expr[String]) => a :+ b
				implicit def AndThenListList:typeclass.Sequenced[List[c.Expr[String]], List[c.Expr[String]], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:List[c.Expr[String]]) => a ++: b
				final class ListRepeatTypes[A] extends typeclass.Repeated[List[A], List[A]] {
					type Acc = scala.collection.mutable.Builder[A, List[A]]
					def init():Acc = List.newBuilder
					def append(acc:Acc, elem:List[A]):Unit = {acc ++= elem}
					def result(acc:Acc):List[A] = acc.result()
				}
				implicit def ListRepeatTypes[A]:typeclass.Repeated[List[A], List[A]] = new ListRepeatTypes[A]
				val EqualsChar = codePointIn("=").map(_.toString).mapToExpr
				val AndChar = codePointIn("&").map(_.toString).mapToExpr

				val tupleConcatFun = q""" {ab:(String, String) => ab._1 + "=" + ab._2} """
				val lit:Interpolator[c.Expr[String]] = (escapedChar orElse unreservedChar orElse codePointIn(";?:@+$,")).repeat().mapToExpr
				val str:Interpolator[c.Expr[String]] = ofType[String]
				val str2:Interpolator[c.Expr[String]] = str orElse lit
				val pair:Interpolator[List[c.Expr[String]]] = ofType(c.typeTag[scala.Tuple2[String, String]])
					.map(x => List(
						c.Expr[String](q"$x._1"),
						constExpr("="),
						c.Expr[String](q"$x._2")
					))
				val pair2:Interpolator[List[c.Expr[String]]] = pair orElse (str2 andThen EqualsChar andThen str2)
				val map:Interpolator[List[c.Expr[String]]] = ofType(c.typeTag[scala.collection.Map[String, String]])
					.map(x => c.Expr[List[String]](q"$x.map($tupleConcatFun)"))
					.map(x => List(c.Expr[String](q""" $x.mkString("&") """)))
				val mapOrPair:Interpolator[List[c.Expr[String]]] = map orElse pair2

				(mapOrPair andThen (AndChar andThen mapOrPair).repeat())
					.map(xs => concatenateStrings(c)(xs))
			}
			Mapping.attempt orElse Arbitrary
		}
		val query:Interpolator[c.Expr[String]] = (isString("?") andThen fragmentOrQueryString).optionally().map(_.getOrElse(constNullExpr))
		val fragment:Interpolator[c.Expr[String]] = (isString("#") andThen fragmentOrQueryString).optionally().map(_.getOrElse(constNullExpr))


		val relativePath:Interpolator[String] =
			(escapedChar orElse unreservedChar orElse codePointIn(";@&=+$,")).repeat(1) andThen absolutePath.optionally()
		val netPath:Interpolator[((c.Expr[String], (c.Expr[String], c.Expr[Int])), c.Expr[String])] = isString("//") andThen server andThen absolutePathExpr
		val noServer:(c.Expr[String], (c.Expr[String], c.Expr[Int])) = (constNullExpr, (constNullExpr, constNegOneExpr))

		val hierarchialPart:Interpolator[(((c.Expr[String], (c.Expr[String], c.Expr[Int])), c.Expr[String]), c.Expr[String])] = {
			(netPath orElse absolutePathExpr.map(x => (noServer, x))) andThen query
		}

		val absoluteUri:Interpolator[c.Expr[URI]] = {
			scheme andThen
			isString(":") flatMap
			({scheme:c.Expr[String] =>
				(isString("//") andThen
					server andThen
					absolutePathExpr.optionally().map(_.getOrElse(constNullExpr)) andThen
					query andThen
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
				}) orElse
				(opaquePart andThen fragment).map({case (ssp, frag) =>
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
				orElse absolutePathExpr.map(x => (noServer, x)).attempt
				orElse relativePath.map(x => (noServer, constExpr(x)))
				andThen query
				andThen fragment
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
			(ofType[URI] andThen relativeUri).map({params =>
				val (base, resolvant) = params
				c.Expr(q"$base.resolve($resolvant)")
			})
		}

		val uri:Interpolator[c.Expr[URI]] = (resolvedUri.attempt orElse absoluteUri.attempt orElse relativeUri) andThen end

		val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.uri.package.UriStringContext"
		uri.interpolate(c)(extensionClassName)(args.toList)
	}
}
