package name.rayrobdod.stringContextParserCombinatorExample.uri

import java.net.URI
import scala.quoted._
import name.rayrobdod.stringContextParserCombinator._
import name.rayrobdod.stringContextParserCombinator.RepeatStrategy._
import name.rayrobdod.stringContextParserCombinatorExample.uri.ConcatenateStringImplicits.{given}

object MacroImpl {
	/**
	 * Creates an Expr that represents the concatenation of the component Exprs
	 */
	private def concatenateStrings(strings:Seq[Expr[String]])(using Quotes):Expr[String] = {
		strings match {
			case Seq() => '{ "" }
			case Seq(x) => x
			case _ => '{ ${Expr.ofSeq(strings)}.mkString }
		}
	}

	import name.rayrobdod.stringContextParserCombinator.Interpolator._
	private def parseByteHex(x:(Char, Char)):Int = java.lang.Integer.parseInt(s"${x._1}${x._2}", 16)


	private def nullExpr(using Quotes):Expr[Null] = '{ null }

	private val hexChar:Interpolator[Char] = charWhere(c => '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F').opaque("hexChar")

	private val alphaChar:Interpolator[CodePoint] = codePointWhere(c => 'a' <= c.intValue && c.intValue <= 'z' || 'A' <= c.intValue && c.intValue <= 'Z').opaque("alphaChar")
	private val digitChar:Interpolator[CodePoint] = codePointWhere(c => '0' <= c.intValue && c.intValue <= '9').opaque("digitChar")
	private val alphaNumChar:Interpolator[CodePoint] = alphaChar orElse digitChar
	private val unreservedChar:Interpolator[CodePoint] = alphaNumChar orElse codePointIn("-_.!~*'()")

	private val escapedChar:Interpolator[CodePoint] = {
		given Utf8ContinuationAndThen:typeclass.Sequenced[Int, Int, Int] = {(a:Int, b:Int) => a << 6 | b}
		val escapedContinuation:Interpolator[Int] = (isString("%") andThen charIn("89ABab") andThen hexChar).map({x => (parseByteHex(x) & 0x3F)})

		(isString("%") andThen (
			(charIn("01234567") andThen hexChar).map({x => CodePoint(parseByteHex(x))}) orElse
			(charIn("cdCD") andThen hexChar).map({x => (parseByteHex(x) & 0x1F)}).andThen(escapedContinuation).map(CodePoint.apply _) orElse
			(charIn("eE") andThen hexChar).map({x => (parseByteHex(x) & 0x0F)}).andThen(escapedContinuation).andThen(escapedContinuation).map(CodePoint.apply _) orElse
			(charIn("fF") andThen charIn("01234567")).map({x => (parseByteHex(x) & 0x07)}).andThen(escapedContinuation).andThen(escapedContinuation).andThen(escapedContinuation).map(CodePoint.apply _)
		))
	}

	private val uriNoSlashChar:Interpolator[CodePoint] = escapedChar orElse unreservedChar orElse codePointIn(";?:@&=+$,")
	private val uriChar:Interpolator[CodePoint] = uriNoSlashChar orElse codePointIn("/")

	private def scheme(using Quotes):Interpolator[Expr[String]] = {
		val literal:Interpolator[Expr[String]] = (alphaChar andThen (alphaNumChar orElse codePointIn("+-.")).repeat()).mapToExpr
		literal
	}

	private def userInfo(using Quotes):Interpolator[Expr[String]] = {
		val literal:Interpolator[Expr[String]] = (unreservedChar orElse escapedChar orElse codePointIn(";:&=+$,")).repeat().mapToExpr
		literal
	}

	private def host(using Quotes):Interpolator[Expr[String]] = {
		val label:Interpolator[String] = alphaNumChar andThen ((alphaNumChar orElse codePointIn("-")).repeat(strategy = Greedy) andThen alphaNumChar).optionally()
		val topLabel:Interpolator[String] = alphaChar andThen ((alphaNumChar orElse codePointIn("-")).repeat(strategy = Greedy) andThen alphaNumChar).optionally()
		val literalName:Interpolator[Expr[String]] = ((label andThen codePointIn(".")).attempt.repeat() andThen topLabel).mapToExpr
		val literalIpv4:Interpolator[Expr[String]] = {
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
		val literalIpv6:Interpolator[Expr[String]] = {
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
		val variableInetAddress:Interpolator[Expr[String]] = ofType[java.net.InetAddress]
			.map(x => '{ $x.getHostName() })
		variableInetAddress orElse literalIpv4 orElse literalIpv6 orElse literalName
	}

	private def port(using Quotes):Interpolator[Expr[Int]] = {
		val literal:Interpolator[Expr[Int]] = digitChar.repeat(1)
			.map({x => java.lang.Integer.parseInt(x)})
			.mapToExpr
		val literalEmpty:Interpolator[Expr[Int]] = isString("").map({_ => Expr.apply(-1)})
		val variable:Interpolator[Expr[Int]] = ofType[Int]
		variable orElse literal orElse literalEmpty
	}

	private def hostPort(using Quotes):Interpolator[(Expr[String], Expr[Int])] = {
		val literal = host andThen (isString(":") andThen port)
			.optionally().map(_.getOrElse(Expr.apply(-1)))
		val SockAddr = ofType[java.net.InetSocketAddress]
			.map(x => (
				'{ $x.getHostString() },
				'{ $x.getPort() },
			))
		SockAddr orElse literal
	}
	private def server(using Quotes):Interpolator[(Expr[String], (Expr[String], Expr[Int]))] =
		(userInfo andThen isString("@")).attempt.optionally().map(_.getOrElse(nullExpr)) andThen hostPort

	private def opaquePart(using Quotes):Interpolator[Expr[String]] = {
		val variable:Interpolator[Expr[String]] = ofType[String]
		val literal:Interpolator[Expr[String]] = (uriNoSlashChar andThen uriChar.repeat()).mapToExpr
		(variable orElse literal).repeat().map(xs => concatenateStrings(xs))
	}


	/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
	private val absolutePath:Interpolator[String] = (codePointIn("/") andThen (unreservedChar orElse escapedChar orElse codePointIn(":@&=+$,;/")).repeat())
	private def absolutePathExpr(using Quotes):Interpolator[Expr[String]] = absolutePath.mapToExpr


	private def fragmentOrQueryString(using Quotes):Interpolator[Expr[String]] = {
		val Arbitrary = (ofType[String] orElse uriChar.repeat(1).mapToExpr)
			.repeat()
			.map(xs => concatenateStrings(xs))
		val Mapping = {
			given typeclass.Sequenced[Expr[String], Expr[String], List[Expr[String]]] = (a, b) => a :: b :: Nil
			given typeclass.Sequenced[Expr[String], List[Expr[String]], List[Expr[String]]] = (a, b) => a +: b
			given typeclass.Sequenced[List[Expr[String]], Expr[String], List[Expr[String]]] = (a, b) => a :+ b
			given typeclass.Sequenced[List[Expr[String]], List[Expr[String]], List[Expr[String]]] = (a, b) => a ++: b
			given [A]:typeclass.Repeated[List[A], List[A]] = new typeclass.Repeated[List[A], List[A]] {
				type Acc = scala.collection.mutable.Builder[A, List[A]]
				def init():Acc = List.newBuilder
				def append(acc:Acc, elem:List[A]):Acc = {acc ++= elem}
				def result(acc:Acc):List[A] = acc.result
			}

			val EqualsChar = codePointIn("=").map(x => Expr.apply(x.toString))
			val AndChar = codePointIn("&").map(x => Expr.apply(x.toString))

			val tupleConcatFun = '{ {(ab:Tuple2[String, String]) => ab._1 + "=" + ab._2} }
			val lit:Interpolator[Expr[String]] = (escapedChar orElse unreservedChar orElse codePointIn(";?:@+$,")).repeat().mapToExpr
			val str:Interpolator[Expr[String]] = ofType[String]
			val str2:Interpolator[Expr[String]] = str orElse lit
			val pair:Interpolator[List[Expr[String]]] = ofType[scala.Tuple2[String, String]]
				.map(x => List(
					'{ $x._1 },
					Expr.apply("="),
					'{ $x._2 },
				))
			val pair2:Interpolator[List[Expr[String]]] = pair orElse (str2 andThen EqualsChar andThen str2)
			val map:Interpolator[List[Expr[String]]] = ofType[scala.collection.Map[String, String]]
				.map(x => '{$x.map($tupleConcatFun)})
				.map(x => List('{ $x.mkString("&") }))
			val mapOrPair:Interpolator[List[Expr[String]]] = map orElse pair2

			(mapOrPair andThen (AndChar andThen mapOrPair).repeat())
				.map(xs => concatenateStrings(xs))
		}
		Mapping.attempt orElse Arbitrary
	}
	private def query(using Quotes):Interpolator[Expr[String|Null]] = (isString("?") andThen fragmentOrQueryString).optionally().map(_.getOrElse(nullExpr))
	private def fragment(using Quotes):Interpolator[Expr[String|Null]] = (isString("#") andThen fragmentOrQueryString).optionally().map(_.getOrElse(nullExpr))


	private val relativePath:Interpolator[String] =
		(escapedChar orElse unreservedChar orElse codePointIn(";@&=+$,")).repeat(1) andThen absolutePath.optionally()
	private def netPath(using Quotes):Interpolator[((Expr[String], (Expr[String], Expr[Int])), Expr[String])] = isString("//") andThen server andThen absolutePathExpr
	private def noServer(using Quotes):(Expr[String], (Expr[String], Expr[Int])) = (nullExpr, (nullExpr, Expr.apply(-1)))

	private def newUriExprOpaque(scheme:Expr[String], ssp:Expr[String], frag:Expr[String])(using Quotes) = '{
		new java.net.URI(
			$scheme,
			$ssp,
			$frag
		)
	}
	private def newUriExprTransparent(scheme:Expr[String], user:Expr[String], host:Expr[String], port:Expr[Int], path:Expr[String], query:Expr[String], fragment:Expr[String])(using Quotes) = '{
		new java.net.URI(
			$scheme,
			$user,
			$host,
			$port,
			$path,
			$query,
			$fragment
		)
	}

	private def absoluteUri(using Quotes):Interpolator[Expr[URI]] = {(
		scheme andThen
		isString(":") flatMap
		({(scheme:Expr[String]) =>
			(isString("//") andThen
				server andThen
				absolutePathExpr.optionally().map(_.getOrElse(nullExpr)) andThen
				query andThen
				fragment
			).map({case ((((user, (host, port)), path), query), fragment) =>
				newUriExprTransparent(scheme, user, host, port, path, query, fragment)}) orElse
			(opaquePart andThen fragment).map({case (ssp, frag) => newUriExprOpaque(scheme, ssp, frag)})
		}) andThen
		end
	)}

	private def relativeUri(using Quotes):Interpolator[Expr[URI]] = {
		((netPath.attempt
			orElse absolutePathExpr.map(x => (noServer, x)).attempt
			orElse relativePath.map(x => (noServer, Expr.apply(x)))
			andThen query
			andThen fragment
			).map({case ((((user, (host, port)), path), query), fragment) =>
				newUriExprTransparent(nullExpr, user, host, port, path, query, fragment)
			})
		)
	}

	private def resolvedUri(using Quotes):Interpolator[Expr[URI]] = {
		(ofType[URI] andThen relativeUri).map({params =>
			val (base, resolvant) = params
			'{ $base.resolve($resolvant) }
		})
	}

	private def uri(using Quotes):Interpolator[Expr[URI]] = (resolvedUri.attempt orElse absoluteUri.attempt orElse relativeUri) andThen end

	def stringContext_uri(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[URI] = {
		uri.interpolate(sc, args)
	}
}
