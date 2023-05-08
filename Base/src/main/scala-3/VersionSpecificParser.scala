package com.rayrobdod.stringContextParserCombinator

import scala.quoted.*
import com.rayrobdod.stringContextParserCombinator.{Parser => SCPCParser}
import com.rayrobdod.stringContextParserCombinator.{Extractor => SCPCExtractor}
import com.rayrobdod.stringContextParserCombinator.{Interpolator => SCPCInterpolator}

/**
 * Parts of [[Parser]] that use types specific to scala 3
 */
private[stringContextParserCombinator]
trait VersionSpecificParser[Expr[_], Type[_], A] {
	protected[stringContextParserCombinator]
	def impl: internal.Parser[Expr, Type, A]

	/**
	 * Parses a StringContext and its arguments into a value
	 *
	 * @example
	 * ```
	 * def valueImpl(sc:Expr[scala.StringContext],
	 *         args:Expr[Seq[Any]])(using Quotes):Expr[Result] = {
	 *   val myParser:Interpolator[Expr[Result]] = ???
	 *   myParser.interpolate(sc, args)
	 * }
	 *
	 * extension (inline sc:scala.StringContext)
	 *	  inline def value(inline args:Any*):Result =
	 *	    ${valueImpl('sc, 'args)}
	 * ```
	 * @group Parse
	 */
	final def interpolate(sc:quoted.Expr[scala.StringContext], args:quoted.Expr[Seq[Any]])(using q:quoted.Quotes, ev:quoted.Expr[Any] <:< Expr[Any]):A = {
		new Interpolator(this.impl).interpolate(sc, args)
	}

	/**
	 * Parses a StringContext into an extractor
	 *
	 * @example
	 * ```
	 * def valueImpl(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[Result]] = {
	 *   val myParser:Extractor[Expr[Result]] = ???
	 *   myParser.extractor(sc)
	 * }
	 *
	 * extension (inline sc:scala.StringContext)
	 *	  inline def value:Unapply[Result] =
	 *	    ${valueImpl('sc)}
	 * ```
	 * @group Parse
	 */
	final def extractor[UnexprA](
		sc:quoted.Expr[scala.StringContext]
	)(using
		quoted.Quotes,
		quoted.Type[UnexprA],
		quoted.Expr[UnexprA] <:< A,
		quoted.Expr[Boolean] =:= Expr[Boolean],
		quoted.Type[Boolean] =:= Type[Boolean],
	):quoted.Expr[Unapply[UnexprA]] = {
		new Extractor(this.impl).extractor(sc)
	}
}

private[stringContextParserCombinator]
trait VersionSpecificParserModule extends ExprIndependentParsers[Expr, Type] {
	type Parser[A] = SCPCParser[quoted.Expr, quoted.Type, A]
	type Extractor[A] = SCPCExtractor[quoted.Expr, quoted.Type, A]
	type Interpolator[A] = SCPCInterpolator[quoted.Expr[Any], A]

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	def OfType[A](using Type[A], Quotes): SCPCParser[Expr, Type, Expr[A]] =
		new SCPCParser(new internal.OfType[A])

	/**
	 * Create an Parsers that can parse `quoted.Expr`s
	 * @group ParserGroup
	 */
	def quotedParsers(using Quotes):Parser.Parsers[Expr, ToExpr, Type] = {
		new Parser.Parsers[Expr, ToExpr, Type]
				with ExprIndependentParsers[Expr, Type] {
			override def DelayedConstruction[A](fn:Function0[SCPCParser[Expr, Type, A]]):SCPCParser[Expr, Type, A] =
				new SCPCParser(internal.DelayedConstruction.parser(() => fn().impl))

			override def Paired[A](interpolator:SCPCInterpolator[Expr[Any], A], extractor:SCPCExtractor[Expr, Type, A]):SCPCParser[Expr, Type, A] =
				new SCPCParser(new internal.Paired(interpolator.impl, extractor.impl))

			override def OfType[A](implicit tpe: Type[A]): SCPCParser[Expr, Type, Expr[A]] =
				new SCPCParser(new internal.OfType[A])
		}
	}
}
