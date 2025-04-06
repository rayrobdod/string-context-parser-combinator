package name.rayrobdod.stringContextParserCombinator

import com.eed3si9n.ifdef.ifdef
import scala.annotation.nowarn
import scala.collection.immutable.Set
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
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
final class Extractor[Expr[_], Type[_], -A] private[stringContextParserCombinator] (
		protected[stringContextParserCombinator] val impl: internal.Extractor[Expr, Type, A]
) {

	/**
	 * Extract subexpressions from the given value according to the given StringContext
	 * @group Parse
	 */
	final def extract(
		sc:StringContext,
		scrutinee:A)(
		implicit
		@nowarn("msg=never used") ev:Id[Any] =:= Expr[Any],
		@nowarn("msg=never used") ev2:ClassTag[Any] =:= Type[Any]
	):Option[Seq[Any]] = {
		implicit val given_Int_Position:Position[Int] = PositionGivens.given_IdPosition_Position

		val argString = "${}"
		val strings = sc.parts.foldLeft((List.empty[(String, Int)], 0)){(folding, part) =>
			val (prevStrings, pos) = folding
			((part, pos) :: prevStrings, pos + part.size + argString.size)
		}._1.reverse
		val argWithPoss = strings.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, Int](strings, argWithPoss)
		implicit val exprs:UnapplyExprs[Id, ClassTag] = UnapplyExprs.forId

		impl.asInstanceOf[internal.Extractor[Id, ClassTag, A]].extractor(input)(implicitly, exprs) match {
			case s:Success[_, _, _] => {
				val expr:UnapplyExpr[Id, ClassTag, A] = s.choicesHead.value
				if (expr.condition(scrutinee)) {
					Some(expr.parts.map(_.value(scrutinee)))
				} else {
					None
				}
			}
			case f:Failure[Int] => {
				val msg = f.expecting match {
					case ExpectingSet.Empty() => "Parsing Failed"
					case set @ ExpectingSet.NonEmpty(position, _) => {
						val exp = set.renderDescriptions
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
	 * Build an extractor that will extract values from a value of type A based on the provided StringContext
	 * @group Parse
	 */
	@ifdef("scalaEpochVersion:2")
	final def extractor[UnexprA](
		c: scala.reflect.macros.blackbox.Context)(
		extensionClassName:String)(
		value:c.Expr[UnexprA])(
		implicit ev:c.Expr[UnexprA] <:< A,
		@nowarn("msg=never used") ev2:c.Expr[_] =:= Expr[_],
		@nowarn("msg=never used") ev3:c.TypeTag[_] =:= Type[_],
		ttUnexprA:c.TypeTag[UnexprA]
	):c.Expr[Any] = {
		implicit val given_Position:Position[c.universe.Position] = PositionGivens.given_ExprPosition_Position(c)
		implicit val given_Ordering:Ordering[c.universe.Position] = PositionGivens.given_ExprPosition_Ordering(c)

		val ExtensionClassSelectChain = selectChain(c, extensionClassName)
		val StringContextApply = stringContextApply(c)

		import c.universe.ApplyTag
		import c.universe.SelectTag
		val strings = c.prefix.tree.duplicate match {
			case c.universe.Apply(
				ExtensionClassSelectChain(),
				List(StringContextApply(strings))
			) => {
				strings.map({x => (c.eval(x), x.tree.pos)})
			}
			case c.universe.Select(
				c.universe.Apply(
					ExtensionClassSelectChain(),
					List(StringContextApply(strings))
				),
				Name(_)
			) => {
				strings.map({x => (c.eval(x), x.tree.pos)})
			}
			case _ => c.abort(c.enclosingPosition, s"Do not know how to process this tree: " + c.universe.showRaw(c.prefix))
		}
		val args = strings.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, c.universe.Position](strings, args)
		implicit val exprs:UnapplyExprs[c.Expr, c.TypeTag] = UnapplyExprs.forContext(c)

		impl.asInstanceOf[internal.Extractor[c.Expr, c.TypeTag, A]].extractor(input)(implicitly, exprs) match {
			case s:Success[_, _, _] => {
				val expr:UnapplyExpr[c.Expr, c.TypeTag, A] = s.choicesHead.value
				val condition = ev.andThen(expr.condition)
				val parts = expr.parts.map(_.contramapValue(ev))

				parts.size match {
					case 0 =>
						AssembleUnapply.zero(c)(value, ttUnexprA, condition)
					case 1 =>
						AssembleUnapply.one(c)(value, ttUnexprA, condition, parts(0))
					case _ =>
						AssembleUnapply.many(c)(value, ttUnexprA, condition, parts)
				}
			}
			case f:Failure[c.universe.Position] => {
				reportFailure(c)(f)
			}
		}
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
	@ifdef("scalaBinaryVersion:3")
	final def extractor[UnexprA](
		sc: scala.quoted.Expr[scala.StringContext]
	)(implicit
		quotes: scala.quoted.Quotes,
		typA: scala.quoted.Type[UnexprA],
		subtupExprA: scala.quoted.Expr[UnexprA] <:< A,
		equalExprBool: scala.quoted.Expr[Boolean] =:= Expr[Boolean],
		equalTypBool: scala.quoted.Type[Boolean] =:= Type[Boolean],
	): scala.quoted.Expr[Unapply[UnexprA]] = {
		import scala.quoted.{Expr => _, quotes => _, _}
		import quotes.reflect.asTerm
		import PositionGivens.given

		val strings = InterpolatorImpl.stringContextFromExpr(sc)
		val strings2 = strings.map(x => ((x.valueOrAbort, x.asTerm.pos))).toList
		val args2 = strings2.init.map(x => (((), x._2 + x._1.size)))

		val input = new Input[Unit, quotes.reflect.Position](strings2, args2)
		implicit val exprs:UnapplyExprs[quoted.Expr, quoted.Type] = UnapplyExprs.forQuoted

		impl.asInstanceOf[internal.Extractor[quoted.Expr, quoted.Type, A]].extractor(input) match {
			case s:Success[_, _, _] => {
				val unexpr = summon[quoted.Expr[UnexprA] <:< A]

				val expr:UnapplyExpr[quoted.Expr, quoted.Type, quoted.Expr[UnexprA]] = unexpr.substituteContra(s.choicesHead.value)

				InterpolatorImpl.unapplyExprToExpr(expr)
			}
			case f:Failure[quotes.reflect.Position] => {
				reportFailure(f)
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
	def widenWith[Z](contrafn: PartialExprFunction[Expr, Z, A]):Extractor[Expr, Type, Z] =
		new Extractor(new internal.WidenWith(this.impl, contrafn))

	/**
	 * Returns an extractor which runs this parser but ignores the input
	 *
	 * Approximately equivalent to `this.contramap({_ => a})`,
	 * where `a` is some value this extractor will accept,
	 * except this discards all match groups
	 * @group Map
	 */
	def void:Extractor[Expr, Type, Unit] =
		new Extractor(internal.Void.extractor(this.impl))

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
	def andThen[B, Z](rhs:Extractor[Expr, Type, B])(implicit ev:typeclass.ContraSequenced[A,B,Z]):Extractor[Expr, Type, Z] =
		new Extractor(internal.AndThen.extractor(this.impl, rhs.impl, ev))

	/**
	 * An alias for [[#andThen]]
	 * @group Sequence
	 * @since 0.1.1
	 */
	def <~>[B, Z](rhs:Extractor[Expr, Type, B])(implicit ev:typeclass.ContraSequenced[A,B,Z]):Extractor[Expr, Type, Z] =
		this.andThen(rhs)(ev)

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser,
	 * discarding the Unit result from the other parser
	 * @group Sequence
	 * @since 0.1.1
	 */
	def <~(rhs:Extractor[Expr, Type, Unit]):Extractor[Expr, Type, A] =
		this.andThen(rhs)(typeclass.ContraSequenced.genericUnit)

	/**
	 * Returns a parser which invokes this parser, and upon success invokes the other parser,
	 * discarding the Unit result from this parser
	 * @group Sequence
	 * @since 0.1.1
	 */
	def ~>[B](rhs:Extractor[Expr, Type, B])(implicit ev: Unit <:< A):Extractor[Expr, Type, B] =
		this.contramap(ev).andThen(rhs)(typeclass.ContraSequenced.unitGeneric)

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
	def orElse[B, Z](rhs:Extractor[Expr, Type, B])(implicit ev:typeclass.ContraEithered[Expr, A,B,Z]):Extractor[Expr, Type, Z] =
		new Extractor(internal.OrElse.extractor(this.impl, rhs.impl, ev))

	/**
	 * An alias for [[#orElse]]
	 * @group Branch
	 * @since 0.1.1
	 */
	def <|>[B, Z](rhs:Extractor[Expr, Type, B])(implicit ev:typeclass.ContraEithered[Expr, A,B,Z]):Extractor[Expr, Type, Z] =
		this.orElse(rhs)(ev)

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
		delimiter:Extractor[Expr, Type, Unit] = new Extractor[Expr, Type, Unit](new internal.Pass),
		strategy:RepeatStrategy = RepeatStrategy.Possessive)(
		implicit ev:typeclass.ContraRepeated[Expr, A, Z]
	):Extractor[Expr, Type, Z] =
		new Extractor(internal.Repeat.extractor(this.impl, min, max, delimiter.impl, strategy, ev))

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
		implicit ev:typeclass.ContraOptionally[Expr, A, Z]
	):Extractor[Expr, Type, Z] =
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
{
	@ifdef("scalaBinaryVersion:3")
	type Extractor[A] = SCExtractor[scala.quoted.Expr, scala.quoted.Type, A]

	/**
	 * Indirectly refers to a parser, to allow for mutual-recursion
	 * @group Misc
	 */
	def `lazy`[Expr[_], Type[_], A](fn:Function0[SCExtractor[Expr, Type, A]]):SCExtractor[Expr, Type, A] =
		new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

	/**
	 * A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree
	 * @group Arg
	 */
	@ifdef("scalaBinaryVersion:3")
	def ofType[A](implicit typA: scala.quoted.Type[A], quotes: scala.quoted.Quotes): SCExtractor[scala.quoted.Expr, scala.quoted.Type, scala.quoted.Expr[A]] =
		new SCExtractor(new internal.OfType[A])

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
	trait Extractors[Expr[+_], Type[_]] {
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
	val idExtractors: Extractors[Id, ClassTag] = {
		new Extractors[Id, ClassTag] with ExprIndependentExtractors[Id, ClassTag] {
			override def `lazy`[A](fn:Function0[this.Extractor[A]]):this.Extractor[A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: ClassTag[A]): this.Extractor[A] =
				new this.Extractor(new internal.OfClass(tpe))
		}
	}

	/**
	 * Create a Extractors that can parse Exprs belonging to the specified Context
	 * @group ExtractorGroup
	 */
	@ifdef("scalaEpochVersion:2")
	def contextExtractors(c:scala.reflect.macros.blackbox.Context):Extractor.Extractors[c.Expr, c.TypeTag] = {
		new Extractor.Extractors[c.Expr, c.TypeTag]
				with ExprIndependentExtractors[c.Expr, c.TypeTag] {
			override def `lazy`[A](fn:Function0[SCExtractor[c.Expr, c.TypeTag, A]]):SCExtractor[c.Expr, c.TypeTag, A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: c.TypeTag[A]): SCExtractor[c.Expr, c.TypeTag, c.Expr[A]] =
				new SCExtractor(new internal.OfType[c.type, A](tpe))
		}
	}

	/**
	 * Create an Extractors that can parse `quoted.Expr`s
	 * @group ExtractorGroup
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedExtractors(implicit quotes: scala.quoted.Quotes):Extractor.Extractors[scala.quoted.Expr, scala.quoted.Type] = {
		import scala.quoted.*
		new Extractor.Extractors[Expr, Type]
				with ExprIndependentExtractors[Expr, Type] {
			override def `lazy`[A](fn:Function0[SCExtractor[Expr, Type, A]]):SCExtractor[Expr, Type, A] =
				new SCExtractor(internal.DelayedConstruction.extractor(() => fn().impl))

			override def ofType[A](implicit tpe: Type[A]): SCExtractor[Expr, Type, Expr[A]] =
				new SCExtractor(new internal.OfType[A])
		}
	}
}

@ifdef("scalaEpochVersion:2")
private[stringContextParserCombinator]
trait VersionSpecificExtractorModule {
}

@ifdef("scalaBinaryVersion:3")
private[stringContextParserCombinator]
trait VersionSpecificExtractorModule extends ExprIndependentExtractors[scala.quoted.Expr, scala.quoted.Type] {
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
