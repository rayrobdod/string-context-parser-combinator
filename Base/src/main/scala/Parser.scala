package name.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import name.rayrobdod.stringContextParserCombinator.{Extractor => SCPCExtractor}
import name.rayrobdod.stringContextParserCombinator.{Interpolator => SCPCInterpolator}
import name.rayrobdod.stringContextParserCombinator.{Parser => SCPCParser}

/**
 * Parses an interpolated string expression into some value
 *
 * @tparam Expr the macro-level expression type
 * @tparam Type the macro-level type type
 * @tparam A the type of the parsed result
 *
 * @groupname Parse parse
 * @groupprio Parse 100
 * @groupname Map Result Changing Combinators
 * @groupprio Map 1010
 * @groupname Sequence Sequencing Combinators
 * @groupprio Sequence 1020
 * @groupname Branch Branching Combinators
 * @groupprio Branch 1030
 * @groupname Filter Filtering Combinators
 * @groupprio Filter 1040
 * @groupname Repeat Repeating Combinators
 * @groupprio Repeat 1050
 * @groupname ErrorPlus Error Enriching Combinators
 * @groupprio ErrorPlus 1060
 * @groupname Misc Other Combinators
 * @groupprio Misc 1999
 * @groupname Convert convert
 * @groupprio Convert 2000
 */
final class Parser[Expr[_], Type[_], A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] override val impl: internal.Parser[Expr, Type, A]
) extends VersionSpecificParser[Expr, Type, A] {

	/**
	 * Returns an Interpolator that interpolates like this parser would
	 * @group Convert
	 */
	def toInterpolator:SCPCInterpolator[Expr[Any], A] =
		new SCPCInterpolator(this.impl)

	/**
	 * Returns an Extractor that builds an extractor like this parser would
	 * @group Convert
	 */
	def toExtractor:SCPCExtractor[Expr, Type, A] =
		new SCPCExtractor(this.impl)

	/**
	 * Processes an immediate string context and its arguments into a value
	 * @group Parse
	 */
	def interpolate(sc:StringContext, args:List[Any])(implicit ev: Any <:< Expr[Any]):A = {
		this.toInterpolator.interpolate(sc, args)
	}

	/**
	 * Extract subexpressions from the given value according to the given StringContext
	 * @group Parse
	 */
	def extract(sc:StringContext, value:A)(implicit ev:Id[Any] =:= Expr[Any], ev2:ClassTag[Any] =:= Type[Any]):Option[Seq[Any]] =
		this.toExtractor.extract(sc, value)

	/**
	 * Returns an interpolator which invokes this parser, then modifies a successful result according to fn
	 * @group Map
	 */
	def map[Z](cofn:A => Z):Interpolator[Expr[Any], Z] =
		new Interpolator(internal.Map.interpolator(this.impl, cofn))

	/**
	 * Returns an extractor which invokes this extractor after mapping the input value using `contrafn`
	 * @group Map
	 */
	def contramap[Z](contrafn: Z => A):Extractor[Expr, Type, Z] =
		new Extractor(internal.Map.extractor(this.impl, contrafn))

	/**
	 * @group Map
	 */
	def imap[Z](cofn:A => Z, contrafn: Z => A):Parser[Expr, Type, Z] =
		new Parser(internal.Map.parser(this.impl, cofn, contrafn))

	/**
	 * Returns an parser which is the pair of an Interpolator#map and an Extractor#widenWith
	 * @group Map
	 */
	def widenWith[Z](cofn:A => Z, contrafn: PartialExprFunction[Expr, Z, A]):Parser[Expr, Type, Z] =
		new Parser[Expr, Type, Z](new internal.Paired[Expr, Type, Z](
			internal.Map.interpolator(this.impl, cofn),
			new internal.WidenWith[Expr, Type, A, Z](this.impl, contrafn)
		))

	/**
	 * Returns a parser
	 * which discards its result while interpolating
	 * and ignores its input value while extracting
	 * @group Map
	 */
	def void:Parser[Expr, Type, Unit] =
		new Parser(internal.Void.parser(this.impl))

	/**
	 * @group Sequence
	 */
	def flatMap[ExprZ <: Expr[Any], Z](cofn:A => Interpolator[ExprZ, Z]):Interpolator[ExprZ, Z] =
		new Interpolator(internal.FlatMap.interpolator(this.impl, cofn))

	/**
	 * Returns a parser which invokes this parser, but has the given description upon failure
	 * @group ErrorPlus
	 */
	def opaque(description:String):Parser[Expr, Type, A] =
		new Parser(internal.Opaque.parser(this.impl, ExpectingDescription(description)))

	/**
	 * Returns a parser which invokes this parser,
	 * but treats the result of a failed parse as if it does not consume input
	 * @group Misc
	 */
	def attempt:Parser[Expr, Type, A] =
		new Parser(internal.Attempt.parser(this.impl))

	/**
	 * Returns a parser which invokes this parser,
	 * but does not show the expected value in failure messages
	 * @group Misc
	 */
	def hide:Parser[Expr, Type, A] =
		new Parser(internal.Hide.parser(this.impl))

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 * @group Sequence
	 */
	def andThen[B, Z](rhs:Parser[Expr, Type, B])(implicit ev:typeclass.BiSequenced[A,B,Z]):Parser[Expr, Type, Z] =
		new Parser(internal.AndThen.parser(this.impl, rhs.impl, ev))

	/**
	 * Returns a parser which invokes this parser, and then:
	 *   * If this parser run succeeded, return this internal's success
	 *   * If this parser failed and consumed input, return this parser's failure
	 *   * If this parser failed but did not consume input, run the other parser and return the other parser's result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to treat either value as one value
	 * @group Branch
	 */
	def orElse[B, Z](rhs:Parser[Expr, Type, B])(implicit ev:typeclass.BiEithered[Expr, A,B,Z]):Parser[Expr, Type, Z] =
		new Parser(internal.OrElse.parser(this.impl, rhs.impl, ev))

	/**
	 * Returns a parser which invokes this parser repeatedly and returns the aggregated result
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param min the minimum number of repeats to be considered successful
	 * @param max the maximum number of repeats to consume
	 * @param delimiter a parser describing separators between each repeat. Defaults to a parser that always succeeds and consumes no input.
	 * @param strategy whether the repeat will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to combine the repeated values into one value
	 * @group Repeat
	 */
	def repeat[Z](
		min:Int = 0,
		max:Int = Integer.MAX_VALUE,
		delimiter:Parser[Expr, Type, Unit] = new Parser[Expr, Type, Unit](new internal.Pass),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.BiRepeated[Expr, A, Z]
	):Parser[Expr, Type, Z] =
		new Parser(internal.Repeat.parser(this.impl, min, max, delimiter.impl, strategy, ev))

	/**
	 * Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param strategy whether the optionally will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to mark present or absent values
	 * @group Repeat
	 */
	def optionally[Z](
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.BiOptionally[Expr, A, Z]
	):Parser[Expr, Type, Z] =
		new Parser(internal.Optionally.parser(this.impl, strategy, ev))
}

/**
 * @groupname ParserGroup ParserGroup
 * @groupprio ParserGroup 3000
 * @groupname Part String-Part
 * @groupprio Part 100
 * @groupname PartAsChar String-Part as Char
 * @groupprio PartAsChar 110
 * @groupname PartAsCodepoint String-Part as Codepoint
 * @groupprio PartAsCodepoint 120
 * @groupname Arg Argument-Part
 * @groupprio Arg 200
 * @groupname Constant Constant
 * @groupprio Constant 300
 * @groupname Position Position
 * @groupprio Position 400
 * @groupname Misc Miscellaneous
 * @groupprio Misc 999
 */
object Parser
		extends VersionSpecificParserModule
{
	/**
	 * A parser that acts like the Interpolator when interpolating, and like the Extractor when extracting
	 * @group Misc
	 */
	def paired[Expr[_], Type[_], A](
		interpolator:SCPCInterpolator[Expr[Any], A],
		extractor:SCPCExtractor[Expr, Type, A]
	):SCPCParser[Expr, Type, A] =
		new SCPCParser[Expr, Type, A](new internal.Paired(interpolator.impl, extractor.impl))

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def `lazy`[Expr[_], Type[_], A](fn:Function0[SCPCParser[Expr, Type, A]]):SCPCParser[Expr, Type, A] =
		new SCPCParser(internal.DelayedConstruction.parser(() => fn().impl))

	// The `ToExpr` tparam isn't used directly, but it does help type inference at use sites
	/**
	 * A trait that provides Parser factory methods that conform to a particular
	 * input Expr type parameter.
	 *
	 * In scala 3, the Parser companion object contains methods similar to these for quoted.Expr,
	 * and as such this would generally by calling methods directly on Parser.
	 * However, since in scala 2 the Expr depends on a particular instance of `blackbox.Context`,
	 * instead an Parsers must be constructed from the Parser companion object's `macroParsers` method that takes a Context.
	 *
	 * @group ParserGroup
	 *
	 * @groupname Part String-Part
	 * @groupprio Part 100
	 * @groupname PartAsChar String-Part as Char
	 * @groupprio PartAsChar 110
	 * @groupname PartAsCodepoint String-Part as Codepoint
	 * @groupprio PartAsCodepoint 120
	 * @groupname Arg Argument-Part
	 * @groupprio Arg 200
	 * @groupname Constant Constant
	 * @groupprio Constant 300
	 * @groupname Position Position
	 * @groupprio Position 400
	 * @groupname Misc Miscellaneous
	 * @groupprio Misc 999
	 */
	trait Parsers[Expr[+_], ToExpr[_], Type[_]] {
		type Interpolator[A] = name.rayrobdod.stringContextParserCombinator.Interpolator[Expr[Any], A]
		type Extractor[A] = name.rayrobdod.stringContextParserCombinator.Extractor[Expr, Type, A]
		type Parser[A] = name.rayrobdod.stringContextParserCombinator.Parser[Expr, Type, A]

		/**
		 * Succeeds if the next character is a member of the given Set; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Set[Char]):Parser[Char]

		/**
		 * Succeeds if the next character is a member of the given Seq; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Seq[Char]):Parser[Char]

		/**
		 * Succeeds if the next character is a member of the given String; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:String):Parser[Char]

		/**
		 * Succeeds if the next character matches the given predicate; captures that character
		 * @group PartAsChar
		 */
		def charWhere(fn:Function1[Char, Boolean]):Parser[Char]

		/**
		 * Succeeds if the next codepoint is a member of the given Set; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Set[CodePoint]):Parser[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Seq[CodePoint]):Parser[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given string; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:String):Parser[CodePoint]

		/**
		 * Succeeds if the next codepoint matches the given predicate; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointWhere(fn:Function1[CodePoint, Boolean]):Parser[CodePoint]

		/**
		 * Succeeds if the next set of characters in the input is equal to the given string
		 * @group Part
		 */
		def isString(str:String):Parser[Unit]

		/**
		 * A parser that consumes no input and always succeeds
		 * @group Constant
		 */
		def pass:Parser[Unit]

		/**
		 * A parser that always reports a failure
		 * @group Constant
		 */
		def fail(message:String):Parser[Nothing]

		/**
		 * A parser that succeeds iff the input is empty
		 * @group Position
		 */
		def end:Parser[Unit]

		/**
		 * Indirectly refers to a parser, to allow for mutual-recursion
		 * @group Misc
		 */
		def `lazy`[A](fn:Function0[Parser[A]]):Parser[A]

		/**
		 * A parser that acts like the Interpolator when interpolating, and like the Extractor when extracting
		 * @group Misc
		 */
		def paired[A](interpolator:Interpolator[A], extractor:Extractor[A]):Parser[A]

		/**
		 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
		 * @group Arg
		 */
		def ofType[A](implicit tpe:Type[A]):Parser[Expr[A]]
	}

	/**
	 * Returns an Parsers that can parse raw values
	 * @group ParserGroup
	 */
	def idParsers: Parsers[Id, IdToExpr, ClassTag] = {
		new Parsers[Id, IdToExpr, ClassTag] with ExprIndependentParsers[Id, ClassTag] {
			override def `lazy`[A](fn:Function0[this.Parser[A]]):this.Parser[A] =
				new this.Parser(internal.DelayedConstruction.parser(() => fn().impl))

			override def paired[A](interpolator:this.Interpolator[A], extractor:this.Extractor[A]):this.Parser[A] =
				new this.Parser(new internal.Paired(interpolator.impl, extractor.impl))

			override def ofType[A](implicit tpe: ClassTag[A]): this.Parser[A] =
				new this.Parser(new internal.OfClass(tpe))
		}
	}
}

/**
 * Parsers that do not introduce an input dependency on Expr
 */
private[stringContextParserCombinator] trait ExprIndependentParsers[Expr[_], Type[_]] {
	/**
	 * Succeeds if the next character is a member of the given Set; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Set[Char]):SCPCParser[Expr, Type, Char] =
		new SCPCParser[Expr, Type, Char](internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given Seq; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Seq[Char]):SCPCParser[Expr, Type, Char] =
		new SCPCParser[Expr, Type, Char](internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given String; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:String):SCPCParser[Expr, Type, Char] =
		new SCPCParser[Expr, Type, Char](internal.CharIn(scala.Predef.wrapString(str)))

	/**
	 * Succeeds if the next character matches the given predicate; captures that character
	 * @group PartAsChar
	 */
	def charWhere(fn:Function1[Char, Boolean]):SCPCParser[Expr, Type, Char] =
		new SCPCParser[Expr, Type, Char](internal.CharWhere(fn))

	/**
	 * Succeeds if the next codepoint is a member of the given Set; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Set[CodePoint]):SCPCParser[Expr, Type, CodePoint] =
		new SCPCParser[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Seq[CodePoint]):SCPCParser[Expr, Type, CodePoint] =
		new SCPCParser[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given string; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:String):SCPCParser[Expr, Type, CodePoint] =
		new SCPCParser[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint matches the given predicate; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointWhere(fn:Function1[CodePoint, Boolean]):SCPCParser[Expr, Type, CodePoint] =
		new SCPCParser[Expr, Type, CodePoint](internal.CodePointWhere(fn))

	/**
	 * Succeeds if the next set of characters in the input is equal to the given string
	 * @group Part
	 */
	def isString(str:String):SCPCParser[Expr, Type, Unit] =
		new SCPCParser[Expr, Type, Unit](internal.IsString(str))

	/**
	 * A parser that consumes no input and always succeeds
	 * @group Constant
	 */
	def pass:SCPCParser[Expr, Type, Unit] =
		new SCPCParser[Expr, Type, Unit](new internal.Pass)

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def fail(message:String):SCPCParser[Expr, Type, Nothing] =
		new SCPCParser[Expr, Type, Nothing](new internal.Fail(ExpectingDescription(message)))

	/**
	 * A parser that succeeds iff the input is empty
	 * @group Position
	 */
	def end:SCPCParser[Expr, Type, Unit] =
		new SCPCParser[Expr, Type, Unit](new internal.End())
}
