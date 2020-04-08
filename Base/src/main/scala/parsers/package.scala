package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

package parsers {
	/** An intermediary class to lessen the weight of implementing Parser repeatedly, Parser being a trait with several concrete methods */
	private[parsers] abstract class AbstractParser[U <: Context with Singleton, +A] extends Parser[U,A]

	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[parsers] final class PartsParser[U <: Context with Singleton, +A](
		partsFn:String => Option[(A, Int)],
		expecting: => Failure.Expecting
	) extends AbstractParser[U, A] {
		def parse(input:Input[U]):Result[U, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}
	}
}

package object parsers {
	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn[U <: Context with Singleton](
		chooseFrom:Seq[Char]
	):Parser[U, Char] = CharWhere(
		chooseFrom.contains _,
		Failure.Or(chooseFrom.map(x => Failure.Leaf("\"" + x.toString + "\"")))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere[U <: Context with Singleton](
		predicate:Function1[Char, Boolean], description:Failure.Expecting
	):Parser[U, Char] = new PartsParser(
		pt => Option((pt.charAt(0), 1)).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn[U <: Context with Singleton](
		chooseFrom:String
	):Parser[U, CodePoint] = {
		def IntEqualsCodePoint(x:CodePoint) = new java.util.function.IntPredicate{def test(y:Int) = {y == x.value}}
		val CodePointString = new java.util.function.IntFunction[String]{def apply(y:Int) = new String(Array[Int]('"', y, '"'), 0, 3)}
		type ToExpectingBuffer = scala.collection.mutable.Builder[Failure.Expecting, Seq[Failure.Expecting]]
		val ToExpecting = new java.util.stream.Collector[String, ToExpectingBuffer, Failure.Expecting]{
			override def supplier = new java.util.function.Supplier[ToExpectingBuffer] {def get = Seq.newBuilder}
			override def accumulator = new java.util.function.BiConsumer[ToExpectingBuffer, String]{def accept(buf:ToExpectingBuffer, a:String) = buf += Failure.Leaf(a)}
			override def combiner = new java.util.function.BinaryOperator[ToExpectingBuffer]{def apply(lhs:ToExpectingBuffer, rhs:ToExpectingBuffer) = {lhs ++= rhs.result; lhs}}
			override def finisher = new java.util.function.Function[ToExpectingBuffer, Failure.Expecting]{def apply(buf:ToExpectingBuffer) = Failure.Or(buf.result)}
			override def characteristics = java.util.Collections.emptySet()
		}

		this.CodePointWhere(
			{x:CodePoint => chooseFrom.codePoints.anyMatch(IntEqualsCodePoint(x))},
			chooseFrom.codePoints.mapToObj(CodePointString).collect(ToExpecting)
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere[U <: Context with Singleton](
		predicate:Function1[CodePoint, Boolean], description:Failure.Expecting
	):Parser[U, CodePoint] = new PartsParser(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next set of characters in the input is equal to the given string */
	private[stringContextParserCombinator]
	def IsString[U <: Context with Singleton](
		value:String
	):Parser[U, Unit] = new PartsParser(
		pt => Option(((), value.length())).filter(_ => pt.startsWith(value)),
		Failure.Leaf("\"" + value + "\"")
	)

	/** Succeeds if the net character data matches the given regex; captures the matched string */
	private[stringContextParserCombinator]
	def Regex[U <: Context with Singleton](
		reg:scala.util.matching.Regex
	):Parser[U, String] = new PartsParser(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		Failure.Leaf("s/" + reg.toString + "/")
	)

	/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
	private[stringContextParserCombinator]
	def OfType[U <: Context with Singleton, A](
		tpetag:U#TypeTag[A]
	):Parser[U, U#Expr[A]] = {
		new OfType(tpetag)
	}

	/** Succeeds only at the end of the given input */
	private[stringContextParserCombinator]
	def End[U <: Context with Singleton](
	):Parser[U, Unit] = {
		new End()
	}

	/* * * Mapping * * */

	private[stringContextParserCombinator]
	def Map[U <: Context with Singleton, A, Z](
		backing:Parser[U,A], mapping:Function1[A, Z]
	):Parser[U, Z] = {
		new Map(backing, mapping)
	}

	private[stringContextParserCombinator]
	def FlatMap[U <: Context with Singleton, A, Z](
		backing:Parser[U, A], mapping:Function1[A, Parser[U, Z]]
	):Parser[U, Z] = {
		new FlatMap(backing, mapping)
	}

	private[stringContextParserCombinator]
	def Filter[U <: Context with Singleton, A](
		backing:Parser[U, A], predicate:Function1[A, Boolean], description:Failure.Expecting
	):Parser[U, A] = {
		new Filter(backing, predicate, description)
	}

	private[stringContextParserCombinator]
	def Opaque[U <: Context with Singleton, A](
		backing:Parser[U, A], description:Failure.Expecting
	):Parser[U, A] = {
		new Opaque(backing, description)
	}

	/** Used to allow mutually recursive parsers */
	private[stringContextParserCombinator]
	def DelayedConstruction[U <: Context with Singleton, A](
		backing:Function0[Parser[U, A]]
	):Parser[U, A] = {
		new DelayedConstruction(backing)
	}

	private[stringContextParserCombinator]
	def Repeat[U <: Context with Singleton, A, Z](
		backing:Parser[U, A],
		min:Int,
		max:Int,
		ev:Implicits.RepeatTypes[A, Z]
	):Parser[U, Z] = {
		new Repeat(backing, min, max, ev)
	}

	private[stringContextParserCombinator]
	def Optionally[U <: Context with Singleton, A, Z](
		backing:Parser[U, A], ev:Implicits.OptionallyTypes[A, Z]
	):Parser[U, Z] = {
		new Repeat(backing, 0, 1, new Implicits.RepeatTypes[A, Z] {
			final class Box[BoxType](var value:BoxType)
			type Acc = Box[Z]
			def init():Acc = new Box(ev.none())
			def append(acc:Acc, elem:A):Unit = acc.value = ev.some(elem)
			def result(acc:Acc):Z = acc.value
		})
	}

	/* * * Combinations * * */

	private[stringContextParserCombinator]
	def AndThen[U <: Context with Singleton, A, B, Z](
		left:Parser[U, A], right:Parser[U, B], ev:Implicits.AndThenTypes[A, B, Z]
	):Parser[U, Z] = {
		new AndThen(left, right, ev)
	}

	private[stringContextParserCombinator]
	def OrElse[U <: Context with Singleton, A](
		left:Parser[U, A], right:Parser[U, A]
	):Parser[U, A] = {
		new OrElse(left, right)
	}
}
