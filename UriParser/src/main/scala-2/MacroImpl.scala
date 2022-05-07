package com.rayrobdod.stringContextParserCombinatorExample.uri

// the compiler creates smaller typecreator classes for `java.lang.String` than `scala.Predef.String`
import java.lang.String

import java.net.URI
import scala.reflect.macros.blackbox.Context
import com.rayrobdod.stringContextParserCombinator._
import com.rayrobdod.stringContextParserCombinatorExample.uri.ConcatinateStringImplicits._

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
		val LeafParsers = Parsers(c)
		import LeafParsers._
		import c.universe.Quasiquote

		val constExpr:Function1[String, c.Expr[String]] = {x => c.Expr(c.universe.Literal(c.universe.Constant(x)))}
		val constNullExpr:c.Expr[Null] = c.Expr(c.universe.Literal(c.universe.Constant(null)))
		val constNegOneExpr:c.Expr[Int] = c.Expr(c.universe.Literal(c.universe.Constant(-1)))
		def parseByteHex(x:(Char, Char)):Int = java.lang.Integer.parseInt(s"${x._1}${x._2}", 16)

		val HexChar:Parser[Char] = CharWhere(c => '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F', "HexChar")

		val AlphaChar:Parser[CodePoint] = CodePointWhere(c => 'a' <= c.value && c.value <= 'z' || 'A' <= c.value && c.value <= 'Z', "AlphaChar")
		val DigitChar:Parser[CodePoint] = CodePointWhere(c => '0' <= c.value && c.value <= '9', "DigitChar")
		val AlphaNumChar:Parser[CodePoint] = AlphaChar orElse DigitChar
		val UnreservedChar:Parser[CodePoint] = AlphaNumChar orElse CodePointIn("-_.!~*'()")

		val EscapedChar:Parser[CodePoint] = {
			implicit object Utf8ContinuationAndThen extends typelevel.Sequenced[Int, Int, Int] {def aggregate(a:Int, b:Int):Int = a << 6 | b}
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
				.map(x => c.Expr(q"$x.getHostName()"))
			VariableInetAddress orElse LiteralIpv4 orElse LiteralIpv6 orElse LiteralName
		}

		val PortP:Parser[c.Expr[Int]] = {
			val Literal:Parser[c.Expr[Int]] = DigitChar.repeat(1)
				.map({x => java.lang.Integer.parseInt(x)})
				.map({x => c.Expr(c.universe.Literal(c.universe.Constant(x)))})
			val LiteralEmpty:Parser[c.Expr[Int]] = IsString("").map({_ => constNegOneExpr})
			val Variable:Parser[c.Expr[Int]] = OfType[Int]
			Variable orElse Literal orElse LiteralEmpty
		}

		val HostPortP:Parser[(c.Expr[String], c.Expr[Int])] = {
			val Literal = HostP andThen (IsString(":") andThen PortP)
				.optionally.map(_.getOrElse(c.Expr(q"-1")))
			val SockAddr = OfType(c.typeTag[java.net.InetSocketAddress])
				.map(x => (
					c.Expr(q"$x.getHostString()"),
					c.Expr(q"$x.getPort()")
				))
			SockAddr orElse Literal
		}
		val ServerP:Parser[(c.Expr[String], (c.Expr[String], c.Expr[Int]))] =
			(UserInfoP andThen IsString("@")).optionally.map(_.getOrElse(constNullExpr)) andThen HostPortP

		val OpaquePartP:Parser[c.Expr[String]] = {
			val Variable:Parser[c.Expr[String]] = OfType[String]
			val Literal:Parser[c.Expr[String]] = (UriNoSlashChar andThen UriChar.repeat()).map(constExpr)
			(Variable orElse Literal).repeat().map(xs => concatenateStrings(c)(xs))
		}


		/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
		val AbsPathP:Parser[String] = (CodePointIn("/") andThen (UnreservedChar orElse EscapedChar orElse CodePointIn(":@&=+$,;/")).repeat())
		val AbsPathExprP:Parser[c.Expr[String]] = AbsPathP.map(constExpr)


		val FragmentOrQueryString:Parser[c.Expr[String]] = {
			val Arbitrary = (OfType[String] orElse UriChar.repeat(1).map(constExpr))
				.repeat()
				.map(xs => concatenateStrings(c)(xs))
			val Mapping = {
				import scala.language.implicitConversions
				implicit def fn2then[A,B,Z](fn:(A,B) => Z):typelevel.Sequenced[A,B,Z] = new typelevel.Sequenced[A,B,Z]{
					def aggregate(a:A, b:B):Z = fn(a,b)
				}
				implicit def AndThenElemElem:typelevel.Sequenced[c.Expr[String], c.Expr[String], List[c.Expr[String]]] = (a:c.Expr[String],b:c.Expr[String]) => a :: b :: Nil
				implicit def AndThenElemList:typelevel.Sequenced[c.Expr[String], List[c.Expr[String]], List[c.Expr[String]]] = (a:c.Expr[String], b:List[c.Expr[String]]) => a +: b
				implicit def AndThenListElem:typelevel.Sequenced[List[c.Expr[String]], c.Expr[String], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:c.Expr[String]) => a :+ b
				implicit def AndThenListList:typelevel.Sequenced[List[c.Expr[String]], List[c.Expr[String]], List[c.Expr[String]]] = (a:List[c.Expr[String]], b:List[c.Expr[String]]) => a ++: b
				final class ListRepeatTypes[A] extends typelevel.Repeated[List[A], List[A]] {
					type Acc = scala.collection.mutable.Builder[A, List[A]]
					def init():Acc = List.newBuilder
					def append(acc:Acc, elem:List[A]):Unit = {acc ++= elem}
					def result(acc:Acc):List[A] = acc.result()
				}
				implicit def ListRepeatTypes[A]:typelevel.Repeated[List[A], List[A]] = new ListRepeatTypes[A]
				val EqualsChar = CodePointIn("=").map(x => constExpr(x.toString))
				val AndChar = CodePointIn("&").map(x => constExpr(x.toString))

				val tupleConcatFun = q""" {ab:(String, String) => ab._1 + "=" + ab._2} """
				val lit:Parser[c.Expr[String]] = (EscapedChar orElse UnreservedChar orElse CodePointIn(";?:@+$,")).repeat().map(constExpr)
				val str:Parser[c.Expr[String]] = OfType[String]
				val str2:Parser[c.Expr[String]] = str orElse lit
				val pair:Parser[List[c.Expr[String]]] = OfType(c.typeTag[scala.Tuple2[String, String]])
					.map(x => List(
						c.Expr[String](q"$x._1"),
						constExpr("="),
						c.Expr[String](q"$x._2")
					))
				val pair2:Parser[List[c.Expr[String]]] = pair orElse (str2 andThen EqualsChar andThen str2)
				val map:Parser[List[c.Expr[String]]] = OfType(c.typeTag[scala.collection.Map[String, String]])
					.map(x => c.Expr[List[String]](q"$x.map($tupleConcatFun)"))
					.map(x => List(c.Expr[String](q""" $x.mkString("&") """)))
				val mapOrPair:Parser[List[c.Expr[String]]] = map orElse pair2

				(mapOrPair andThen (AndChar andThen mapOrPair).repeat())
					.map(xs => concatenateStrings(c)(xs))
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
				(OpaquePartP andThen FragmentP).map({case (ssp, frag) =>
					c.Expr(q"""
						new java.net.URI(
							$scheme,
							$ssp,
							$frag
						)
					""")
				})
			}) andThen
			End
		}

		val RelativeUriP:Parser[c.Expr[URI]] = {
			((NetPathP
				orElse AbsPathExprP.map(x => (noServer, x))
				orElse RelPathP.map(x => (noServer, constExpr(x)))
				andThen QueryP
				andThen FragmentP
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

		val ResolvedUriP:Parser[c.Expr[URI]] = {
			(OfType[URI] andThen RelativeUriP).map({params =>
				val (base, resolvant) = params
				c.Expr(q"$base.resolve($resolvant)")
			})
		}

		val Aggregate:Parser[c.Expr[URI]] = (ResolvedUriP orElse AbsoluteUriP orElse RelativeUriP) andThen End

		val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.uri.package.UriStringContext"
		macroimpl(c)(extensionClassName, Aggregate)(args.toList)
	}
}
