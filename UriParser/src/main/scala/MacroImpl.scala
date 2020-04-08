package com.rayrobdod.stringContextParserCombinatorExample.uri

import java.net.URI
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

object MacroImpl {
	def stringContext_uri(c:Context {type PrefixType = UriStringContext})(args:c.Expr[Any]*):c.Expr[URI] = {
		object ParserPieces extends Parsers{
			type ContextType = c.type
			val constExpr:Function1[String, c.Expr[String]] = {x => c.Expr(c.universe.Literal(c.universe.Constant(x)))}
			val constNullExpr:c.Expr[Null] = c.Expr(c.universe.Literal(c.universe.Constant(null)))
			val constNegOneExpr:c.Expr[Int] = c.Expr(c.universe.Literal(c.universe.Constant(-1)))
			def parseByteHex(x:(Char, Char)):Int = java.lang.Integer.parseInt(s"${x._1}${x._2}", 16)


			implicit object AndThenCodepointString extends Implicits.AndThenTypes[CodePoint, String, String] {
				def aggregate(a:CodePoint, b:String):String = s"${a}${b}"
			}
			implicit object AndThenStringCodepoint extends Implicits.AndThenTypes[String, CodePoint, String] {
				def aggregate(a:String, b:CodePoint):String = s"${a}${b}"
			}
			implicit object AndThenStringString extends Implicits.AndThenTypes[String, String, String] {
				def aggregate(a:String, b:String):String = s"${a}${b}"
			}
			implicit object EmptyStringOptionallyTypes extends Implicits.OptionallyTypes[String, String] {
				def none():String = ""
				def some(elem:String):String = elem
			}
			implicit object CodePointOptionallyTypes extends Implicits.OptionallyTypes[CodePoint, String] {
				def none():String = ""
				def some(elem:CodePoint):String = elem.toString
			}
			implicit object StringRepeatTypes extends Implicits.RepeatTypes[String, String] {
				type Acc = StringBuilder
				def init():Acc = new StringBuilder
				def append(acc:Acc, elem:String):Unit = {acc ++= elem}
				def result(acc:Acc):String = acc.toString
			}

			val HexChar:Parser[Char] = CharWhere(c => '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F', "HexChar")

			val AlphaChar:Parser[CodePoint] = CodePointWhere(c => 'a' <= c.value && c.value <= 'z' || 'A' <= c.value && c.value <= 'Z', "AlphaChar")
			val DigitChar:Parser[CodePoint] = CodePointWhere(c => '0' <= c.value && c.value <= '9', "DigitChar")
			val AlphaNumChar:Parser[CodePoint] = AlphaChar orElse DigitChar
			val UnreservedChar:Parser[CodePoint] = AlphaNumChar orElse CodePointIn("-_.!~*'()")

			val EscapedChar:Parser[CodePoint] = {
				implicit object Utf8ContinuationAndThen extends Implicits.AndThenTypes[Int, Int, Int] {def aggregate(a:Int, b:Int):Int = a << 6 | b}
				val EscapedContinuation:Parser[Int] = (IsString("%") andThen CharIn("89ABab") andThen HexChar).map({x => (parseByteHex(x) & 0x3F)})

				(IsString("%") andThen (
					(CharIn("01234567") andThen HexChar).map({x => CodePoint(parseByteHex(x))}) orElse
					(CharIn("cdCD") andThen HexChar).map({x => (parseByteHex(x) & 0x1F)}).andThen(EscapedContinuation).map(CodePoint.apply _) orElse
					(CharIn("eE") andThen HexChar).map({x => (parseByteHex(x) & 0x0F)}).andThen(EscapedContinuation).andThen(EscapedContinuation).map(CodePoint.apply _) orElse
					(CharIn("fF") andThen CharIn("01234567")).map({x => (parseByteHex(x) & 0x07)}).andThen(EscapedContinuation).andThen(EscapedContinuation).andThen(EscapedContinuation).map(CodePoint.apply _)
				))
			}

			val UriNoSlashChar:Parser[CodePoint] = EscapedChar orElse UnreservedChar orElse CodePointIn(";?:@&=+$,")
			val UriChar:Parser[CodePoint] = UriNoSlashChar orElse CodePointIn("/")

			val SchemeP:Parser[c.Expr[String]] = {
				val Literal:Parser[c.Expr[String]] = (AlphaChar andThen (AlphaNumChar orElse CodePointIn("+-.")).repeat()).map(constExpr)
				Literal
			}

			val UserInfoP:Parser[c.Expr[String]] = {
				val Literal:Parser[c.Expr[String]] = (UnreservedChar orElse EscapedChar orElse CodePointIn(";:&=+$,")).repeat().map(constExpr)
				Literal
			}

			val HostP:Parser[c.Expr[String]] = {
				val label:Parser[String] = AlphaNumChar andThen ((AlphaNumChar orElse CodePointIn("-")).repeat() andThen AlphaNumChar).optionally
				val topLabel:Parser[String] = AlphaChar andThen ((AlphaNumChar orElse CodePointIn("-")).repeat() andThen AlphaNumChar).optionally
				val LiteralName:Parser[c.Expr[String]] = ((label andThen CodePointIn(".")).repeat() andThen topLabel).map(constExpr)
				val LiteralIpv4:Parser[c.Expr[String]] = {
					val Segment:Parser[String] = (
						IsString("0").map(_ => "0") orElse
							(CodePointIn("1") andThen DigitChar.repeat(0,2)) orElse
							(CodePointIn("2") andThen (
								(CodePointIn("01234") andThen DigitChar.optionally) orElse
								(CodePointIn("5") andThen CodePointIn("012345").optionally) orElse
								(CodePointIn("6789").map(_.toString))
							).optionally) orElse
							(CodePointIn("3456789") andThen DigitChar.optionally)
					)
					(Segment andThen (CodePointIn(".") andThen Segment).repeat(3,3)).map(constExpr).opaque("IPv4 Address")
				}
				val LiteralIpv6:Parser[c.Expr[String]] = {
					val Segment:Parser[String] = HexChar.repeat(1,4)
					val ColonSegment:Parser[String] = CodePointIn(":") andThen Segment
					val SegmentColon:Parser[String] = Segment andThen CodePointIn(":")

					val Regex:Parser[String] = CodePointIn("[") andThen (
						(CodePointIn(":") andThen (ColonSegment.repeat(1, 7) orElse CodePointIn(":").map(_.toString))) orElse
						(SegmentColon andThen (
							(ColonSegment andThen ColonSegment.repeat(0, 6)) orElse
							(SegmentColon andThen (
								(ColonSegment andThen ColonSegment.repeat(0, 5)) orElse
								(SegmentColon andThen (
									(ColonSegment andThen ColonSegment.repeat(0, 4)) orElse
									(SegmentColon andThen (
										(ColonSegment andThen ColonSegment.repeat(0, 3)) orElse
										(SegmentColon andThen (
											(ColonSegment andThen ColonSegment.repeat(0, 2)) orElse
											(SegmentColon andThen (
												(ColonSegment andThen ColonSegment.repeat(0, 1)) orElse
												(Segment andThen ColonSegment)
											))
										))
									))
								))
							))
						))
					) andThen CodePointIn("]")
					Regex.map(constExpr).opaque("IPv6 Address")
				}
				/* Luckily, the URI constructor seems to be able to surround v6 addresses in brackets automatically, so that we don't have to */
				val VariableInetAddress:Parser[c.Expr[String]] = OfType(c.typeTag[java.net.InetAddress])
					.map(x => c.universe.reify(x.splice.getHostName()))
				VariableInetAddress orElse LiteralIpv4 orElse LiteralIpv6 orElse LiteralName
			}

			val PortP:Parser[c.Expr[Int]] = {
				val Literal:Parser[c.Expr[Int]] = DigitChar.repeat(1)
					.map({x => java.lang.Integer.parseInt(x)})
					.map({x => c.Expr(c.universe.Literal(c.universe.Constant(x)))})
				val LiteralEmpty:Parser[c.Expr[Int]] = IsString("").map({_ => constNegOneExpr})
				val Variable:Parser[c.Expr[Int]] = OfType(c.typeTag[Int])
				Variable orElse Literal orElse LiteralEmpty
			}

			val HostPortP:Parser[(c.Expr[String], c.Expr[Int])] = {
				val Literal = HostP andThen (IsString(":") andThen PortP)
					.optionally.map(_.getOrElse(c.universe.reify(-1)))
				val SockAddr = OfType(c.typeTag[java.net.InetSocketAddress])
					.map(x => (
						c.universe.reify(x.splice.getHostString()),
						c.universe.reify(x.splice.getPort())
					))
				SockAddr orElse Literal
			}
			val ServerP:Parser[(c.Expr[String], (c.Expr[String], c.Expr[Int]))] =
				(UserInfoP andThen IsString("@")).optionally.map(_.getOrElse(constNullExpr)) andThen HostPortP

			val OpaquePartP:Parser[c.Expr[String]] = {
				val Variable:Parser[c.Expr[String]] = OfType(c.typeTag[String])
				val Literal:Parser[c.Expr[String]] = (UriNoSlashChar andThen UriChar.repeat()).map(constExpr)
				(Variable orElse Literal).repeat().map(xs => Utilities.concatenateStrings(c)(xs))
			}


			/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
			val AbsPathP:Parser[String] = (CodePointIn("/") andThen (UnreservedChar orElse EscapedChar orElse CodePointIn(":@&=+$,;/")).repeat())
			val AbsPathExprP:Parser[c.Expr[String]] = AbsPathP.map(constExpr)


			val FragmentOrQueryString:Parser[c.Expr[String]] = {
				val Arbitrary = (OfType(c.typeTag[String]) orElse UriChar.repeat(1).map(constExpr))
					.repeat()
					.map(xs => Utilities.concatenateStrings(c)(xs))
				val Mapping = {
					import scala.language.implicitConversions
					implicit def fn2then[A,B,Z](fn:(A,B) => Z):Implicits.AndThenTypes[A,B,Z] = new Implicits.AndThenTypes[A,B,Z]{
						def aggregate(a:A, b:B):Z = fn(a,b)
					}
					implicit def AndThenElemElem:Implicits.AndThenTypes[c.Expr[String], c.Expr[String], List[c.Expr[String]]] = (a:c.Expr[String],b:c.Expr[String]) => a :: b :: Nil
					implicit def AndThenElemList:Implicits.AndThenTypes[c.Expr[String], List[c.Expr[String]], List[c.Expr[String]]] = (a:c.Expr[String], b:List[c.Expr[String]]) => a +: b
					implicit def AndThenListElem:Implicits.AndThenTypes[List[c.Expr[String]], c.Expr[String], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:c.Expr[String]) => a :+ b
					implicit def AndThenListList:Implicits.AndThenTypes[List[c.Expr[String]], List[c.Expr[String]], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:List[c.Expr[String]]) => a ++: b
					final class ListRepeatTypes[A] extends Implicits.RepeatTypes[List[A], List[A]] {
						type Acc = scala.collection.mutable.Builder[A, List[A]]
						def init():Acc = List.newBuilder
						def append(acc:Acc, elem:List[A]):Unit = {acc ++= elem}
						def result(acc:Acc):List[A] = acc.result
					}
					implicit def ListRepeatTypes[A]:Implicits.RepeatTypes[List[A], List[A]] = new ListRepeatTypes[A]
					val EqualsChar = CodePointIn("=").map(x => constExpr(x.toString))
					val AndChar = CodePointIn("&").map(x => constExpr(x.toString))

					val tupleConcatFun = c.universe.reify( {ab:Tuple2[String, String] => ab._1 + "=" + ab._2} )
					val lit:Parser[c.Expr[String]] = (EscapedChar orElse UnreservedChar orElse CodePointIn(";?:@+$,")).repeat().map(constExpr)
					val str:Parser[c.Expr[String]] = OfType(c.typeTag[String])
					val str2:Parser[c.Expr[String]] = str orElse lit
					val pair:Parser[List[c.Expr[String]]] = OfType(c.typeTag[scala.Tuple2[String, String]])
						.map(x => List(
							c.universe.reify(x.splice._1),
							constExpr("="),
							c.universe.reify(x.splice._2)
						))
					val pair2:Parser[List[c.Expr[String]]] = pair orElse (str2 andThen EqualsChar andThen str2)
					val map:Parser[List[c.Expr[String]]] = OfType(c.typeTag[scala.collection.Map[String, String]])
						.map(x => c.universe.reify(x.splice.map(tupleConcatFun.splice)))
						.map(x => List(c.universe.reify(x.splice.mkString("&"))))
					val mapOrPair:Parser[List[c.Expr[String]]] = map orElse pair2

					(mapOrPair andThen (AndChar andThen mapOrPair).repeat())
						.map(xs => Utilities.concatenateStrings(c)(xs))
				}
				Mapping orElse Arbitrary
			}
			val QueryP:Parser[c.Expr[String]] = (IsString("?") andThen FragmentOrQueryString).optionally.map(_.getOrElse(constNullExpr))
			val FragmentP:Parser[c.Expr[String]] = (IsString("#") andThen FragmentOrQueryString).optionally.map(_.getOrElse(constNullExpr))


			val RelPathP:Parser[String] =
				(EscapedChar orElse UnreservedChar orElse CodePointIn(";@&=+$,")).repeat(1) andThen AbsPathP.optionally
			val NetPathP:Parser[((c.Expr[String], (c.Expr[String], c.Expr[Int])), c.Expr[String])] = IsString("//") andThen ServerP andThen AbsPathExprP
			val noServer:(c.Expr[String], (c.Expr[String], c.Expr[Int])) = (constNullExpr, (constNullExpr, constNegOneExpr))

			val HierarchialPart:Parser[(((c.Expr[String], (c.Expr[String], c.Expr[Int])), c.Expr[String]), c.Expr[String])] = {
				(NetPathP orElse AbsPathExprP.map(x => (noServer, x))) andThen QueryP
			}

			val AbsoluteUriP:Parser[c.Expr[URI]] = {
				SchemeP andThen
				IsString(":") flatMap
				({scheme:c.Expr[String] =>
					(IsString("//") andThen
						(UserInfoP andThen IsString("@")).optionally.map(_.getOrElse(constNullExpr)) andThen
						HostPortP andThen
						AbsPathExprP.optionally.map(_.getOrElse(constNullExpr)) andThen
						QueryP andThen
						FragmentP
					).map({case ((((user, (host, port)), path), query), fragment) =>
						c.universe.reify(
							new java.net.URI(
								scheme.splice,
								user.splice,
								host.splice,
								port.splice,
								path.splice,
								query.splice,
								fragment.splice
							)
						)
					}) orElse
					(OpaquePartP andThen FragmentP).map({case (ssp, frag) =>
						c.universe.reify(
							new java.net.URI(
								scheme.splice,
								ssp.splice,
								frag.splice
							)
						)
					})
				}) andThen
				End()
			}

			val RelativeUriP:Parser[c.Expr[URI]] = {
				((NetPathP
					orElse AbsPathExprP.map(x => (noServer, x))
					orElse RelPathP.map(x => (noServer, constExpr(x)))
					andThen QueryP
					andThen FragmentP
					).map({case ((((user, (host, port)), path), query), fragment) =>
						c.universe.reify(
							new java.net.URI(
								constNullExpr.splice,
								user.splice,
								host.splice,
								port.splice,
								path.splice,
								query.splice,
								fragment.splice
							)
						)
					})
				)
			}

			val ResolvedUriP:Parser[c.Expr[URI]] = {
				(OfType(c.typeTag[URI]) andThen RelativeUriP)map({params =>
					val (base, resolvant) = params
					c.universe.reify(base.splice.resolve(resolvant.splice))
				})
			}

			val Aggregate:Parser[c.Expr[URI]] = (ResolvedUriP orElse AbsoluteUriP orElse RelativeUriP) andThen End
		}

		val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.uri.package.UriStringContext"
		macroimpl(c)(extensionClassName, ParserPieces.Aggregate)(args.toList)
	}
}
