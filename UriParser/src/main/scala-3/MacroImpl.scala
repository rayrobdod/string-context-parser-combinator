package com.rayrobdod.stringContextParserCombinatorExample.uri

import java.net.URI
import scala.quoted._
import com.rayrobdod.stringContextParserCombinator._

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

	import com.rayrobdod.stringContextParserCombinator.Parsers._
	private def parseByteHex(x:(Char, Char)):Int = java.lang.Integer.parseInt(s"${x._1}${x._2}", 16)


	private def nullExpr(using Quotes):Expr[Null] = '{ null }
	private implicit val AndThenCodepointString:typelevel.Sequenced[CodePoint, String, String] = {
		(a:CodePoint, b:String) => s"${a}${b}"
	}
	private implicit val AndThenStringCodepoint:typelevel.Sequenced[String, CodePoint, String] = {
		(a:String, b:CodePoint) => s"${a}${b}"
	}
	private implicit val AndThenStringString:typelevel.Sequenced[String, String, String] = {
		(a:String, b:String) => s"${a}${b}"
	}
	private implicit object EmptyStringOptionallyTypes extends typelevel.Optionally[String, String] {
		def none:String = ""
		def some(elem:String):String = elem
	}
	private implicit object CodePointOptionallyTypes extends typelevel.Optionally[CodePoint, String] {
		def none:String = ""
		def some(elem:CodePoint):String = elem.toString
	}
	private implicit object StringRepeatTypes extends typelevel.Repeated[String, String] {
		type Acc = StringBuilder
		def init():Acc = new StringBuilder
		def append(acc:Acc, elem:String):Unit = {acc ++= elem}
		def result(acc:Acc):String = acc.toString
	}

	private val HexChar:Parser[Char] = CharWhere(c => '0' <= c && c <= '9' || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F', "HexChar")

	private val AlphaChar:Parser[CodePoint] = CodePointWhere(c => 'a' <= c.value && c.value <= 'z' || 'A' <= c.value && c.value <= 'Z', "AlphaChar")
	private val DigitChar:Parser[CodePoint] = CodePointWhere(c => '0' <= c.value && c.value <= '9', "DigitChar")
	private val AlphaNumChar:Parser[CodePoint] = AlphaChar orElse DigitChar
	private val UnreservedChar:Parser[CodePoint] = AlphaNumChar orElse CodePointIn("-_.!~*'()")

	private val EscapedChar:Parser[CodePoint] = {
		implicit val Utf8ContinuationAndThen:typelevel.Sequenced[Int, Int, Int] = {(a:Int, b:Int) => a << 6 | b}
		val EscapedContinuation:Parser[Int] = (IsString("%") andThen CharIn("89ABab") andThen HexChar).map({x => (parseByteHex(x) & 0x3F)})

		(IsString("%") andThen (
			(CharIn("01234567") andThen HexChar).map({x => CodePoint(parseByteHex(x))}) orElse
			(CharIn("cdCD") andThen HexChar).map({x => (parseByteHex(x) & 0x1F)}).andThen(EscapedContinuation).map(CodePoint.apply _) orElse
			(CharIn("eE") andThen HexChar).map({x => (parseByteHex(x) & 0x0F)}).andThen(EscapedContinuation).andThen(EscapedContinuation).map(CodePoint.apply _) orElse
			(CharIn("fF") andThen CharIn("01234567")).map({x => (parseByteHex(x) & 0x07)}).andThen(EscapedContinuation).andThen(EscapedContinuation).andThen(EscapedContinuation).map(CodePoint.apply _)
		))
	}

	private val UriNoSlashChar:Parser[CodePoint] = EscapedChar orElse UnreservedChar orElse CodePointIn(";?:@&=+$,")
	private val UriChar:Parser[CodePoint] = UriNoSlashChar orElse CodePointIn("/")

	private def SchemeP(using Quotes):Parser[Expr[String]] = {
		val Literal:Parser[Expr[String]] = (AlphaChar andThen (AlphaNumChar orElse CodePointIn("+-.")).repeat()).map(Expr.apply _)
		Literal
	}

	private def UserInfoP(using Quotes):Parser[Expr[String]] = {
		val Literal:Parser[Expr[String]] = (UnreservedChar orElse EscapedChar orElse CodePointIn(";:&=+$,")).repeat().map(Expr.apply _)
		Literal
	}

	private def HostP(using Quotes):Parser[Expr[String]] = {
		val label:Parser[String] = AlphaNumChar andThen ((AlphaNumChar orElse CodePointIn("-")).repeat() andThen AlphaNumChar).optionally
		val topLabel:Parser[String] = AlphaChar andThen ((AlphaNumChar orElse CodePointIn("-")).repeat() andThen AlphaNumChar).optionally
		val LiteralName:Parser[Expr[String]] = ((label andThen CodePointIn(".")).repeat() andThen topLabel).map(Expr.apply _)
		val LiteralIpv4:Parser[Expr[String]] = {
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
			(Segment andThen (CodePointIn(".") andThen Segment).repeat(3,3)).map(Expr.apply _).opaque(Expecting("IPv4 Address"))
		}
		val LiteralIpv6:Parser[Expr[String]] = {
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
			Regex.map(Expr.apply _).opaque(Expecting("IPv6 Address"))
		}
		/* Luckily, the URI constructor seems to be able to surround v6 addresses in brackets automatically, so that we don't have to */
		val VariableInetAddress:Parser[Expr[String]] = OfType[java.net.InetAddress]
			.map(x => '{ $x.getHostName() })
		VariableInetAddress orElse LiteralIpv4 orElse LiteralIpv6 orElse LiteralName
	}

	private def PortP(using Quotes):Parser[Expr[Int]] = {
		val Literal:Parser[Expr[Int]] = DigitChar.repeat(1)
			.map({x => java.lang.Integer.parseInt(x)})
			.map(Expr.apply _)
		val LiteralEmpty:Parser[Expr[Int]] = IsString("").map({_ => Expr.apply(-1)})
		val Variable:Parser[Expr[Int]] = OfType[Int]
		Variable orElse Literal orElse LiteralEmpty
	}

	private def HostPortP(using Quotes):Parser[(Expr[String], Expr[Int])] = {
		val Literal = HostP andThen (IsString(":") andThen PortP)
			.optionally.map(_.getOrElse(Expr.apply(-1)))
		val SockAddr = OfType[java.net.InetSocketAddress]
			.map(x => (
				'{ $x.getHostString() },
				'{ $x.getPort() },
			))
		SockAddr orElse Literal
	}
	private def ServerP(using Quotes):Parser[(Expr[String], (Expr[String], Expr[Int]))] =
		(UserInfoP andThen IsString("@")).optionally.map(_.getOrElse(nullExpr)) andThen HostPortP

	private def OpaquePartP(using Quotes):Parser[Expr[String]] = {
		val Variable:Parser[Expr[String]] = OfType[String]
		val Literal:Parser[Expr[String]] = (UriNoSlashChar andThen UriChar.repeat()).map(Expr.apply _)
		(Variable orElse Literal).repeat().map(xs => concatenateStrings(xs))
	}


	/* We don't really care about the structure of the absolute path, so don't bother with the Segments / Segment / Param / ParamC subparsers */
	private val AbsPathP:Parser[String] = (CodePointIn("/") andThen (UnreservedChar orElse EscapedChar orElse CodePointIn(":@&=+$,;/")).repeat())
	private def AbsPathExprP(using Quotes):Parser[Expr[String]] = AbsPathP.map(Expr.apply _)


	private def FragmentOrQueryString(using Quotes):Parser[Expr[String]] = {
		val Arbitrary = (OfType[String] orElse UriChar.repeat(1).map(Expr.apply _))
			.repeat()
			.map(xs => concatenateStrings(xs))
		val Mapping = {
			import scala.language.implicitConversions
			implicit def fn2then[A,B,Z](fn:(A,B) => Z):typelevel.Sequenced[A,B,Z] = new typelevel.Sequenced[A,B,Z]{
				def aggregate(a:A, b:B):Z = fn(a,b)
			}
			implicit def AndThenElemElem:typelevel.Sequenced[Expr[String], Expr[String], List[Expr[String]]] = (a:Expr[String],b:Expr[String]) => a :: b :: Nil
			implicit def AndThenElemList:typelevel.Sequenced[Expr[String], List[Expr[String]], List[Expr[String]]] = (a:Expr[String], b:List[Expr[String]]) => a +: b
			implicit def AndThenListElem:typelevel.Sequenced[List[Expr[String]], Expr[String], List[Expr[String]]] = (a:List[Expr[String]], b:Expr[String]) => a :+ b
			implicit def AndThenListList:typelevel.Sequenced[List[Expr[String]], List[Expr[String]], List[Expr[String]]] = (a:List[Expr[String]], b:List[Expr[String]]) => a ++: b
			final class ListRepeatTypes[A] extends typelevel.Repeated[List[A], List[A]] {
				type Acc = scala.collection.mutable.Builder[A, List[A]]
				def init():Acc = List.newBuilder
				def append(acc:Acc, elem:List[A]):Unit = {acc ++= elem}
				def result(acc:Acc):List[A] = acc.result
			}
			implicit def ListRepeatTypes[A]:typelevel.Repeated[List[A], List[A]] = new ListRepeatTypes[A]
			val EqualsChar = CodePointIn("=").map(x => Expr.apply(x.toString))
			val AndChar = CodePointIn("&").map(x => Expr.apply(x.toString))

			val tupleConcatFun = '{ {(ab:Tuple2[String, String]) => ab._1 + "=" + ab._2} }
			val lit:Parser[Expr[String]] = (EscapedChar orElse UnreservedChar orElse CodePointIn(";?:@+$,")).repeat().map(Expr.apply _)
			val str:Parser[Expr[String]] = OfType[String]
			val str2:Parser[Expr[String]] = str orElse lit
			val pair:Parser[List[Expr[String]]] = OfType[scala.Tuple2[String, String]]
				.map(x => List(
					'{ $x._1 },
					Expr.apply("="),
					'{ $x._2 },
				))
			val pair2:Parser[List[Expr[String]]] = pair orElse (str2 andThen EqualsChar andThen str2)
			val map:Parser[List[Expr[String]]] = OfType[scala.collection.Map[String, String]]
				.map(x => '{$x.map($tupleConcatFun)})
				.map(x => List('{ $x.mkString("&") }))
			val mapOrPair:Parser[List[Expr[String]]] = map orElse pair2

			(mapOrPair andThen (AndChar andThen mapOrPair).repeat())
				.map(xs => concatenateStrings(xs))
		}
		Mapping orElse Arbitrary
	}
	private def QueryP(using Quotes):Parser[Expr[String|Null]] = (IsString("?") andThen FragmentOrQueryString).optionally.map(_.getOrElse(nullExpr))
	private def FragmentP(using Quotes):Parser[Expr[String|Null]] = (IsString("#") andThen FragmentOrQueryString).optionally.map(_.getOrElse(nullExpr))


	private val RelPathP:Parser[String] =
		(EscapedChar orElse UnreservedChar orElse CodePointIn(";@&=+$,")).repeat(1) andThen AbsPathP.optionally
	private def NetPathP(using Quotes):Parser[((Expr[String], (Expr[String], Expr[Int])), Expr[String])] = IsString("//") andThen ServerP andThen AbsPathExprP
	private def noServer(using Quotes):(Expr[String], (Expr[String], Expr[Int])) = (nullExpr, (nullExpr, Expr.apply(-1)))

	private def HierarchialPart(using Quotes):Parser[(((Expr[String], (Expr[String], Expr[Int])), Expr[String]), Expr[String])] = {
		(NetPathP orElse AbsPathExprP.map(x => (noServer, x))) andThen QueryP
	}

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

	private def AbsoluteUriP(using Quotes):Parser[Expr[URI]] = {
		SchemeP andThen
		IsString(":") flatMap
		({(scheme:Expr[String]) =>
			(IsString("//") andThen
				(UserInfoP andThen IsString("@")).optionally.map(_.getOrElse(nullExpr)) andThen
				HostPortP andThen
				AbsPathExprP.optionally.map(_.getOrElse(nullExpr)) andThen
				QueryP andThen
				FragmentP
			).map({case ((((user, (host, port)), path), query), fragment) =>
				newUriExprTransparent(scheme, user, host, port, path, query, fragment)}) orElse
			(OpaquePartP andThen FragmentP).map({case (ssp, frag) => newUriExprOpaque(scheme, ssp, frag)})
		}) andThen
		End()
	}

	private def RelativeUriP(using Quotes):Parser[Expr[URI]] = {
		((NetPathP
			orElse AbsPathExprP.map(x => (noServer, x))
			orElse RelPathP.map(x => (noServer, Expr.apply(x)))
			andThen QueryP
			andThen FragmentP
			).map({case ((((user, (host, port)), path), query), fragment) =>
				newUriExprTransparent(nullExpr, user, host, port, path, query, fragment)
			})
		)
	}

	private def ResolvedUriP(using Quotes):Parser[Expr[URI]] = {
		(OfType[URI] andThen RelativeUriP)map({params =>
			val (base, resolvant) = params
			'{ $base.resolve($resolvant) }
		})
	}

	private def Aggregate(using Quotes):Parser[Expr[URI]] = (ResolvedUriP orElse AbsoluteUriP orElse RelativeUriP) andThen End()

	def stringContext_uri(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[URI] = {
		macroimpl[URI](Aggregate)(sc, args)
	}
}
