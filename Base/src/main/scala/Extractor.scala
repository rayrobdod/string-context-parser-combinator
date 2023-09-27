package name.rayrobdod.stringContextParserCombinator

import scala.annotation.nowarn
import scala.collection.immutable.Set
import scala.collection.immutable.Seq
import name.rayrobdod.stringContextParserCombinator.{Extractor => SCExtractor}

/**
 * Parses an interpolated string expression into some extractor
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
 */
final class Extractor[+Expr[_], +Type[_], -A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] override val impl: internal.Extractor[Expr, Type, A]
) extends VersionSpecificExtractor[Expr, Type, A] {

	/**
	 * Extract subexpressions from the given value according to the given StringContext
	 * @group Parse
	 */
	final def extract(
		sc:StringContext,
		scrutinee:A)(
		implicit
		@nowarn("msg=never used") ev:Expr[Any] <:< Id[Any],
		@nowarn("msg=never used") ev2:Type[Any] <:< Class[Any],
	):Option[Seq[Any]] = {
		def unapplyExprEval[Z](value:Z, expr:UnapplyExpr[Id, Class, Z]):Option[List[Any]] = {
			expr match {
				case UnapplyExpr.Empty => Some(Nil)
				case UnapplyExpr.IsEqualTo(other, typ@_) => if (value == other) {Some(Nil)} else {None}
				case UnapplyExpr.OfType(typ@_) => Some(value :: Nil)
				case UnapplyExpr.Contramap(backing, mapping) => {
					unapplyExprEval(mapping(value), backing)
				}
				case UnapplyExpr.WidenWith(backing, mapping) => {
					if (mapping.isDefinedAt(value)) {
						unapplyExprEval(mapping(value), backing)
					} else {
						None
					}
				}
				case UnapplyExpr.OptionallyNone(ev) => {
					if (ev.contraNone(value)) {
						Some(Nil)
					} else {
						None
					}
				}
				case UnapplyExpr.Sequenced(leftBacking, rightBacking, ev) => {
					val (leftValue, rightValue) = ev.separate(value)

					(unapplyExprEval(leftValue, leftBacking), unapplyExprEval(rightValue, rightBacking)) match {
						case ((Some(a), Some(b))) => Some(a ::: b)
						case _ => None
					}
				}
				case UnapplyExpr.Repeated(childBackings, ev2) => {
					// I do not know why, for Repeated specifically, the typer drops the Z and decides that it is Any instead
					val ev = ev2.asInstanceOf[typeclass.ContraRepeated[Id, Any, Z]]

					val (finalRetval, finalValue) = childBackings.foldLeft[(Option[List[Any]], ev.Dec)](
						(Some(Nil), ev.contraInit(value))
					)({(folding, childBacking) =>
						val (foldingRetvalOpt, foldingValue) = folding
						foldingRetvalOpt match {
							case None => (None, foldingValue)
							case Some(foldingRetval) => {
								val stopCondition = ev.headTail.isDefinedAt(foldingValue)

								if (!stopCondition) {
									(None, foldingValue)
								} else {
									val (child, newValue) = ev.headTail.apply(foldingValue)

									val childRetval = unapplyExprEval(child, childBacking.asInstanceOf[UnapplyExpr[Id, Class, Any]])

									((
										childRetval.map(_ reverse_::: foldingRetval),
										newValue
									))
								}
							}
						}
					})

					if (ev.isEmpty(finalValue)) {
						finalRetval.map(_.reverse)
					} else {
						None
					}
				}
			}
		}


		implicit val given_Int_Position:Position[Int] = PositionGivens.given_IdPosition_Position

		val argString = "${}"
		val strings = sc.parts.foldLeft((List.empty[(String, Int)], 0)){(folding, part) =>
			val (prevStrings, pos) = folding
			((part, pos) :: prevStrings, pos + part.size + argString.size)
		}._1.reverse
		val argWithPoss = strings.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, Int](strings, argWithPoss)

		impl.asInstanceOf[internal.Extractor[Id, Class, A]].extractor(input) match {
			case s:Success[_, _, _] => {
				val expr:UnapplyExpr[Id, Class, A] = s.choicesHead.value
				unapplyExprEval(scrutinee, expr)
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
	 * Returns an extractor which invokes this extractor after mapping the input value using `contrafn`
	 * @group Map
	 */
	def contramap[Z](contrafn:Function1[Z, A]):Extractor[Expr, Type, Z] =
		new Extractor(internal.Map.extractor(this.impl, contrafn))

	/**
	 * Returns an extractor which invokes the contrafn, then
	 *   * If the Expr is true, passes the value to this extractor
	 *   * If the Expr is false, fails the match
	 * @group Map
	 */
	def widenWith[ExprZ[x] >: Expr[x], Z](contrafn: PartialExprFunction[ExprZ, Z, A]):Extractor[ExprZ, Type, Z] =
		new Extractor(new internal.WidenWith(this.impl, contrafn))

	/**
	 * Returns a extractor which invokes this parser, but has the given description upon failure
	 * @group ErrorPlus
	 */
	def opaque(description:String):Extractor[Expr, Type, A] =
		new Extractor(internal.Opaque.extractor(this.impl, ExpectingDescription(description)))

	/**
	 * Returns a parser which invokes this parser,
	 * but treats the result of a failed parse as if it does not consume input
	 * @group Misc
	 */
	def attempt:Extractor[Expr, Type, A] =
		new Extractor(internal.Attempt.extractor(this.impl))

	/**
	 * Returns a parser which invokes this parser,
	 * but does not show the expected value in failure messages
	 * @group Misc
	 */
	def hide:Extractor[Expr, Type, A] =
		new Extractor(internal.Hide.extractor(this.impl))

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser.
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param rhs the parser to call after this one
	 * @param ev A descriptor of how to combine two values into one value
	 * @group Sequence
	 */
	def andThen[ExprZ[x] >: Expr[x], TypeZ[x] >: Type[x], B, Z](rhs:Extractor[ExprZ, TypeZ, B])(implicit ev:typeclass.ContraSequenced[A,B,Z]):Extractor[ExprZ, TypeZ, Z] =
		new Extractor(internal.AndThen.extractor(this.impl, rhs.impl, ev))

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
	def orElse[ExprZ[x] >: Expr[x], TypeZ[x] >: Type[x], B, Z](rhs:Extractor[ExprZ, TypeZ, B])(implicit ev:typeclass.ContraEithered[ExprZ, A,B,Z]):Extractor[ExprZ, TypeZ, Z] =
		new Extractor(internal.OrElse.extractor(this.impl, rhs.impl, ev))

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
	def repeat[ExprZ[x] >: Expr[x], TypeZ[x] >: Type[x], Z](
		min:Int = 0,
		max:Int = Integer.MAX_VALUE,
		delimiter:Extractor[ExprZ, TypeZ, Unit] = new Extractor[ExprZ, TypeZ, Unit](new internal.Pass),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.ContraRepeated[ExprZ, A, Z]
	):Extractor[ExprZ, TypeZ, Z] =
		new Extractor(internal.Repeat.extractor(this.impl, min, max, delimiter.impl, strategy, ev))

	/**
	 * Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed
	 *
	 * @tparam Z the result parser's parsed value type
	 * @param strategy whether the optionally will attempt to match as much or as little as possible, and whether it will backtrack. Default is [[RepeatStrategy.Possessive]]
	 * @param ev A descriptor of how to mark present or absent values
	 * @group Repeat
	 */
	def optionally[ExprZ[x] >: Expr[x], Z](
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.ContraOptionally[ExprZ, A, Z]
	):Extractor[ExprZ, Type, Z] =
		new Extractor(internal.Optionally.extractor(this.impl, strategy, ev))
}

/**
 * @groupname ExtractorGroup ExtractorGroup
 * @groupprio ExtractorGroup 3000
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
object Extractor
		extends VersionSpecificExtractorModule
		with ExprIndependentExtractors[Nothing, Nothing]
{
	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def `lazy`[Expr[_], Type[_], A](fn:Function0[SCExtractor[Expr, Type, A]]):SCExtractor[Expr, Type, A] =
		new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

	/**
	 * A trait that provides Extractor factory methods that conform to a particular
	 * input Expr type parameter.
	 *
	 * In scala 3, the Extractor companion object contains methods similar to these for quoted.Expr,
	 * and as such this would generally by calling methods directly on Extractor.
	 * However, since in scala 2 the Expr depends on a particular instance of `blackbox.Context`,
	 * instead an Extractors must be constructed from the Extractor companion object's `macroExtractors` method that takes a Context.
	 *
	 * @group ExtractorGroup
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
	trait Extractors[Expr[_], Type[_]] {
		type Extractor[A] = name.rayrobdod.stringContextParserCombinator.Extractor[Expr, Type, A]

		/**
		 * Succeeds if the next character is a member of the given Set; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Set[Char]):Extractor[Char]

		/**
		 * Succeeds if the next character is a member of the given Seq; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:Seq[Char]):Extractor[Char]

		/**
		 * Succeeds if the next character is a member of the given String; captures that character
		 * @group PartAsChar
		 */
		def charIn(str:String):Extractor[Char]

		/**
		 * Succeeds if the next character matches the given predicate; captures that character
		 * @group PartAsChar
		 */
		def charWhere(fn:Function1[Char, Boolean]):Extractor[Char]

		/**
		 * Succeeds if the next codepoint is a member of the given Set; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Set[CodePoint]):Extractor[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:Seq[CodePoint]):Extractor[CodePoint]

		/**
		 * Succeeds if the next codepoint is a member of the given string; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointIn(str:String):Extractor[CodePoint]

		/**
		 * Succeeds if the next codepoint matches the given predicate; captures that code point
		 * @group PartAsCodepoint
		 */
		def codePointWhere(fn:Function1[CodePoint, Boolean]):Extractor[CodePoint]

		/**
		 * Succeeds if the next set of characters in the input is equal to the given string
		 * @group Part
		 */
		def isString(str:String):Extractor[Unit]

		/**
		 * A parser that consumes no input and always succeeds
		 * @group Constant
		 */
		def pass:Extractor[Unit]

		/**
		 * A parser that always reports a failure
		 * @group Constant
		 */
		def fail(message:String):Extractor[Nothing]

		/**
		 * A parser that succeeds iff the input is empty
		 * @group Position
		 */
		def end:Extractor[Unit]

		/**
		 * Indirectly refers to a parser, to allow for mutual-recursion
		 * @group Misc
		 */
		def `lazy`[A](fn:Function0[Extractor[A]]):Extractor[A]

		/**
		 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
		 * @group Arg
		 */
		def ofType[A](implicit tpe:Type[A]):Extractor[Expr[A]]
	}

	/**
	 * Returns an Extractors that can parse raw values
	 * @group ExtractorGroup
	 */
	def idExtractors: Extractors[Id, Class] = {
		new Extractors[Id, Class] with ExprIndependentExtractors[Id, Class] {
			override def `lazy`[A](fn:Function0[this.Extractor[A]]):this.Extractor[A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: Class[A]): this.Extractor[A] =
				new this.Extractor(new internal.OfClass(tpe))
		}
	}
}

/**
 * Extractors that do not introduce an input dependency on Expr
 */
private[stringContextParserCombinator] trait ExprIndependentExtractors[Expr[_], Type[_]] {
	/**
	 * Succeeds if the next character is a member of the given Set; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Set[Char]):SCExtractor[Expr, Type, Char] =
		new SCExtractor[Expr, Type, Char](internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given Seq; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:Seq[Char]):SCExtractor[Expr, Type, Char] =
		new SCExtractor[Expr, Type, Char](internal.CharIn(str))

	/**
	 * Succeeds if the next character is a member of the given String; captures that character
	 * @group PartAsChar
	 */
	def charIn(str:String):SCExtractor[Expr, Type, Char] =
		new SCExtractor[Expr, Type, Char](internal.CharIn(scala.Predef.wrapString(str)))

	/**
	 * Succeeds if the next character matches the given predicate; captures that character
	 * @group PartAsChar
	 */
	def charWhere(fn:Function1[Char, Boolean]):SCExtractor[Expr, Type, Char] =
		new SCExtractor[Expr, Type, Char](internal.CharWhere(fn))

	/**
	 * Succeeds if the next codepoint is a member of the given Set; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Set[CodePoint]):SCExtractor[Expr, Type, CodePoint] =
		new SCExtractor[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given Seq; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:Seq[CodePoint]):SCExtractor[Expr, Type, CodePoint] =
		new SCExtractor[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint is a member of the given string; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointIn(str:String):SCExtractor[Expr, Type, CodePoint] =
		new SCExtractor[Expr, Type, CodePoint](internal.CodePointIn(str))

	/**
	 * Succeeds if the next codepoint matches the given predicate; captures that code point
	 * @group PartAsCodepoint
	 */
	def codePointWhere(fn:Function1[CodePoint, Boolean]):SCExtractor[Expr, Type, CodePoint] =
		new SCExtractor[Expr, Type, CodePoint](internal.CodePointWhere(fn))

	/**
	 * Succeeds if the next set of characters in the input is equal to the given string
	 * @group Part
	 */
	def isString(str:String):SCExtractor[Expr, Type, Unit] =
		new SCExtractor[Expr, Type, Unit](internal.IsString(str))

	/**
	 * A parser that consumes no input and always succeeds
	 * @group Constant
	 */
	def pass:SCExtractor[Expr, Type, Unit] =
		new SCExtractor[Expr, Type, Unit](new internal.Pass)

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def fail(message:String):SCExtractor[Expr, Type, Nothing] =
		new SCExtractor[Expr, Type, Nothing](new internal.Fail(ExpectingDescription(message)))

	/**
	 * A parser that succeeds iff the input is empty
	 * @group Position
	 */
	def end:SCExtractor[Expr, Type, Unit] =
		new SCExtractor[Expr, Type, Unit](new internal.End())
}
