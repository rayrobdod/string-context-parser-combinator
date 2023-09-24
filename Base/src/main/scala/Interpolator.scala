package name.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Set
import scala.collection.immutable.Seq
import name.rayrobdod.stringContextParserCombinator.{Interpolator => SCPCInterpolator}

/**
 * Parses an interpolated string expression into some value
 *
 * @tparam Expr the macro-level expression type.
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
 */
final class Interpolator[-Expr, +A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] override val impl: internal.Interpolator[Expr, A]
) extends VersionSpecificInterpolator[Expr, A] {

	/**
	 * Processes an immediate string context and its arguments into a value
	 * @example
	 * ```
	 * extension (sc:StringContext)
	 *   def prefix(args:Any*):Result =
	 *     val interpolator:Interpolator[Result] = ???
	 *     interpolator.interpolate(sc, args)
	 * ```
	 * @group Parse
	 */
	def interpolate(sc:StringContext, args:Seq[Any])(implicit ev: Any <:< Expr):A = {
		implicit val given_Int_Position:Position[Int] = PositionGivens.given_IdPosition_Position

		val argString = "${}"
		val strings = sc.parts.foldLeft((List.empty[(String, Int)], 0)){(folding, part) =>
			val (prevStrings, pos) = folding
			((part, pos) :: prevStrings, pos + part.size + argString.size)
		}._1.reverse
		val argWithPoss = args.zip(strings).map({(argStrPos) =>
			val (arg, (str, pos)) = argStrPos
			(ev(arg), pos + str.size)
		}).toList

		val input = new Input[Expr, Int](strings, argWithPoss)

		impl.interpolate(input) match {
			case s:Success[_, _, _] => {
				s.choicesHead.value
			}
			case f:Failure[Int] => {
				val msg = f.expecting match {
					case ExpectingSet.Empty() => "Parsing Failed"
					case ExpectingSet.NonEmpty(position, descriptions) => {
						// `sorted` to make result deterministic
						val exp = descriptions.toList.sortBy(_.toString).mkString("Expected ", " or ", "")
						val instr = sc.parts.mkString(argString)
						val pointer = (" " * position) + "^"

						s"$exp\n\t$instr\n\t$pointer"
					}
				}
				throw new ParseException(msg)
			}
		}
	}



	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to fn
	 * @group Map
	 */
	def map[Z](fn:Function1[A, Z]):Interpolator[Expr, Z] =
		new Interpolator(internal.Map.interpolator(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then maps a successful result by lifting the
	 * successful result into an Expr
	 * @group Map
	 */
	def mapToExpr[Z >: A, Expr2[_], ToExpr[_], Type[_]](
		implicit mapping:typeclass.ToExprMapping[Expr2, ToExpr, Type],
		toExpr:ToExpr[Z],
		tpe:Type[Z]
	):Interpolator[Expr, Expr2[Z]] = {
		this.map(value => mapping(value, toExpr, tpe))
	}

	/**
	 * Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn
	 * @group Sequence
	 */
	def flatMap[ExprZ <: Expr, Z](fn:Function1[A, Interpolator[ExprZ, Z]]):Interpolator[ExprZ, Z] =
		new Interpolator(internal.FlatMap.interpolator(this.impl, fn))

	/**
	 * Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate
	 * @group Filter
	 */
	def filter(predicate:Function1[A, Boolean], description:String):Interpolator[Expr, A] =
		new Interpolator(internal.Filter.interpolator(this.impl, predicate, ExpectingDescription(description)))


	/**
	 * Returns a parser which invokes this parser, but has the given description upon failure
	 * @group ErrorPlus
	 */
	def opaque(description:String):Interpolator[Expr, A] =
		new Interpolator(internal.Opaque.interpolator(this.impl, ExpectingDescription(description)))

	/**
	 * Returns a parser which invokes this parser,
	 * but treats the result of a failed parse as if it does not consume input
	 * @group Misc
	 */
	def attempt:Interpolator[Expr, A] =
		new Interpolator(internal.Attempt.interpolator(this.impl))

	/**
	 * Returns a parser which invokes this parser,
	 * but does not show the expected value in failure messages
	 * @group Misc
	 */
	def hide:Interpolator[Expr, A] =
		new Interpolator(internal.Hide.interpolator(this.impl))

	/**
	 * Creates a parser that will
	 *    * when interpolating, act like this parser
	 *    * when extractoring, invoke this parser and check that the extractor input is equal to the parsed value
	 *
	 * The extractor parsing will probably fail if this parser expects to find holes.
	 * @group Misc
	 */
	def extractorAtom[ExprZ[_], TypeZ[_], UnexprA](
		implicit t:TypeZ[UnexprA],
		ev:ExprZ[Any] <:< Expr,
		ev2:A <:< ExprZ[UnexprA]
	):Parser[ExprZ, TypeZ, ExprZ[UnexprA]] = {
		type T2[-X] = internal.Interpolator[X, A]
		type T3[+X] = internal.Interpolator[ExprZ[Any], X]

		val impl2 = TypeConformanceCompat.contraSubstituteContra[T2, ExprZ[Any], Expr](ev, this.impl)
		val impl3 = TypeConformanceCompat.contraSubstituteCo[T3, A, ExprZ[UnexprA]](ev2, impl2)
		new Parser(new internal.ExtractorAtom[ExprZ, TypeZ, UnexprA](impl3, t))
	}

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 * @group Sequence
	 */
	def andThen[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Sequenced[A,B,Z]):Interpolator[ExprZ, Z] =
		new Interpolator(internal.AndThen.interpolator(this.impl, rhs.impl, ev))

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
	def orElse[ExprZ <: Expr, B, Z](rhs:Interpolator[ExprZ, B])(implicit ev:typeclass.Eithered[A,B,Z]):Interpolator[ExprZ, Z] =
		new Interpolator(internal.OrElse.interpolator(this.impl, rhs.impl, ev))

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
	def repeat[ExprZ <: Expr, Z](
		min:Int = 0,
		max:Int = Integer.MAX_VALUE,
		delimiter:Interpolator[ExprZ, Unit] = new Interpolator[ExprZ, Unit](new internal.Pass[Id, Id]),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.Repeated[A, Z]
	):Interpolator[ExprZ, Z] =
		new Interpolator(internal.Repeat.interpolator(this.impl, min, max, delimiter.impl, strategy, ev))

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
		implicit ev:typeclass.Optionally[A, Z]
	):Interpolator[Expr, Z] =
		new Interpolator(internal.Optionally.interpolator(this.impl, strategy, ev))
}

/**
 * @groupname InterpolatorGroup InterpolatorGroup
 * @groupprio InterpolatorGroup 3000
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
object Interpolator
		extends VersionSpecificInterpolatorModule
		with ExprIndependentInterpolators[Any]
{
	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def `lazy`[Expr, A](fn:Function0[SCPCInterpolator[Expr, A]]):SCPCInterpolator[Expr, A] =
		new SCPCInterpolator(internal.DelayedConstruction.interpolator(fn))

	// The `ToExpr` tparam isn't used directly, but it does help type inference at use sites
	/**
	 * A trait that provides Interpolator factory methods that conform to a particular
	 * input Expr type parameter.
	 *
	 * In scala 3, the Interpolator companion object contains methods similar to these for quoted.Expr,
	 * and as such this would generally by calling methods directly on Interpolator.
	 * However, since in scala 2 the Expr depends on a particular instance of `blackbox.Context`,
	 * instead an Interpolators must be constructed from the Interpolator companion object's `macroInterpolators` method that takes a Context.
	 *
	 * @group InterpolatorGroup
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
	trait Interpolators[Expr[+_], ToExpr[_], Type[_]] {
		type Interpolator[A] = name.rayrobdod.stringContextParserCombinator.Interpolator[Expr[Any], A]

		/**
		 * Succeeds if the next character is a member of the given Set; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Set[Char]):Interpolator[Char]

		/**
		 * Succeeds if the next character is a member of the given Seq; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Seq[Char]):Interpolator[Char]

		/**
		 * Succeeds if the next character is a member of the given String; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:String):Interpolator[Char]

		/**
		 * Succeeds if the next character matches the given predicate; captures that character
		 * @group PartAsChar
		 */
		def charWhere(fn:Function1[Char, Boolean]):Interpolator[Char]

		/**
		 * Succeeds if the next codepoint is a member of the given Set; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Set[CodePoint]):Interpolator[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Seq[CodePoint]):Interpolator[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given string; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:String):Interpolator[CodePoint]

		/**
		 * Succeeds if the next codepoint matches the given predicate; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointWhere(fn:Function1[CodePoint, Boolean]):Interpolator[CodePoint]

		/**
		 * Succeeds if the next set of characters in the input is equal to the given string
		 * @group Part
		 */
		def isString(str:String):Interpolator[Unit]

		/**
		 * A parser that consumes no input and always succeeds
		 * @group Constant
		 */
		def pass:Interpolator[Unit]

		/**
		 * A parser that always reports a failure
		 * @group Constant
		 */
		def fail(message:String):Interpolator[Nothing]

		/**
		 * A parser that succeeds iff the input is empty
		 * @group Position
		 */
		def end:Interpolator[Unit]

		/**
		 * Indirectly refers to a parser, to allow for mutual-recursion
		 * @group Misc
		 */
		def `lazy`[A](fn:Function0[Interpolator[A]]):Interpolator[A]

		/**
		 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
		 * @group Arg
		 */
		def ofType[A](implicit tpe:Type[A]):Interpolator[Expr[A]]
	}

	/**
	 * Returns an Interpolators that can parse raw values
	 * @group InterpolatorGroup
	 */
	val idInterpolators: Interpolators[Id, IdToExpr, Class] = {
		new Interpolators[Id, IdToExpr, Class] with ExprIndependentInterpolators[Any] {
			override def `lazy`[A](fn:Function0[SCPCInterpolator[Any, A]]):SCPCInterpolator[Any, A] =
				new SCPCInterpolator(internal.DelayedConstruction.interpolator(fn))

			override def ofType[A](implicit tpe: Class[A]): this.Interpolator[A] =
				new this.Interpolator(new internal.OfClass(tpe))
		}
	}
}

/**
 * Interpolators that do not introduce an input dependency on Expr
 */
private[stringContextParserCombinator] trait ExprIndependentInterpolators[Expr] {
	/**
	 * Succeeds if the next character is a member of the given Set; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Set[Char]):SCPCInterpolator[Expr, Char] =
		new SCPCInterpolator[Expr, Char](internal.CharIn[Id, Id](str))

	/**
	 * Succeeds if the next character is a member of the given Seq; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Seq[Char]):SCPCInterpolator[Expr, Char] =
		new SCPCInterpolator[Expr, Char](internal.CharIn[Id, Id](str))

	/**
	 * Succeeds if the next character is a member of the given String; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:String):SCPCInterpolator[Expr, Char] =
		new SCPCInterpolator[Expr, Char](internal.CharIn[Id, Id](scala.Predef.wrapString(str)))

	/**
	 * Succeeds if the next character matches the given predicate; captures that character
	 * @group PartAsChar
	 */
	def charWhere(fn:Function1[Char, Boolean]):SCPCInterpolator[Expr, Char] =
		new SCPCInterpolator[Expr, Char](internal.CharWhere[Id, Id](fn))

	/**
	 * Succeeds if the next codepoint is a member of the given Set; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Set[CodePoint]):SCPCInterpolator[Expr, CodePoint] =
		new SCPCInterpolator[Expr, CodePoint](internal.CodePointIn[Id, Id](str))

	/**
	 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Seq[CodePoint]):SCPCInterpolator[Expr, CodePoint] =
		new SCPCInterpolator[Expr, CodePoint](internal.CodePointIn[Id, Id](str))

	/**
	 * Succeeds if the next codepoint is a member of the given string; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:String):SCPCInterpolator[Expr, CodePoint] =
		new SCPCInterpolator[Expr, CodePoint](internal.CodePointIn[Id, Id](str))

	/**
	 * Succeeds if the next codepoint matches the given predicate; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointWhere(fn:Function1[CodePoint, Boolean]):SCPCInterpolator[Expr, CodePoint] =
		new SCPCInterpolator[Expr, CodePoint](internal.CodePointWhere[Id, Id](fn))

	/**
	 * Succeeds if the next set of characters in the input is equal to the given string
	 * @group Part
	 */
	def isString(str:String):SCPCInterpolator[Expr, Unit] =
		new SCPCInterpolator[Expr, Unit](internal.IsString[Id, Id](str))

	/**
	 * A parser that consumes no input and always succeeds
	 * @group Constant
	 */
	def pass:SCPCInterpolator[Expr, Unit] =
		new SCPCInterpolator[Expr, Unit](new internal.Pass[Id, Id])

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def fail(message:String):SCPCInterpolator[Expr, Nothing] =
		new SCPCInterpolator[Expr, Nothing](new internal.Fail[Id, Id](ExpectingDescription(message)))

	/**
	 * A parser that succeeds iff the input is empty
	 * @group Position
	 */
	def end:SCPCInterpolator[Expr, Unit] =
		new SCPCInterpolator[Expr, Unit](new internal.End[Id, Id]())
}
