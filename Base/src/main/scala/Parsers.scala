package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * @group Parser
 */
trait Parser[U <: Context with Singleton, +A] {
	def parse(input:Input[U]):Result[U, A]

	def map[Z](fn:Function1[A, Z]):Parser[U, Z] = new Parser[U, Z] {
		def parse(input:Input[U]):Result[U, Z] = Parser.this.parse(input).map(fn)
	}
	def flatMap[Z](fn:Function1[A, Parser[U, Z]]):Parser[U, Z] = {
		new FlatMap(this, fn)
	}

	def opaque(description:String):Parser[U, A] = new Parser[U, A] {
		def parse(input:Input[U]):Result[U, A] = {
			Parser.this.parse(input) match {
				case Success(v, r) => Success(v,r)
				case Failure(f, _) => Failure(f, Failure.Leaf(description))
			}
		}
	}

	def andThen[B, Z](rhs:Parser[U, B])(implicit ev:Implicits.AndThenTypes[A,B,Z]):Parser[U, Z] = {
		new AndThen(this, rhs, ev)
	}
	def orElse[Z >: A](rhs:Parser[U, Z]):Parser[U, Z] = new Parser[U, Z] {
		def parse(input:Input[U]):Result[U, Z] = Parser.this.parse(input).orElse(rhs.parse(input))
	}
	def repeat[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[A, Z]):Parser[U, Z] = new Repeat(this, min, max, ev)

	def optionally[Z](implicit ev:Implicits.OptionallyTypes[A, Z]):Parser[U, Z] = new Repeat(this, 0, 1, new Implicits.RepeatTypes[A, Z] {
		final class Box[BoxType](var value:BoxType)
		type Acc = Box[Z]
		def init():Acc = new Box(ev.none())
		def append(acc:Acc, elem:A):Unit = acc.value = ev.some(elem)
		def result(acc:Acc):Z = acc.value
	})
}

/**
 * A mixin that provides Parser factory methods that conform to a particular `U` parameter.
 *
 * Exists to reduce the need to repeatedly explicitly specify `c.type` as the U parameter,
 * since the compiler seems to be not great at inferring a Singleton type
 *
 * And you'd need to put the parsers in an object anyway if you wanted to make recursive
 * parsers, so one is probably paying the cost required of the mixin anyway.
 */
trait Parsers {
	import com.rayrobdod.{stringContextParserCombinator => scpc}
	type ContextType <: Context with Singleton
	type Parser[A] = scpc.Parser[ContextType, A]
	def CharIn(str:Seq[Char]):Parser[Char] = new scpc.CharIn[ContextType](str)
	def CharIn(str:String):Parser[Char] = new scpc.CharIn[ContextType](scala.Predef.wrapString(str))
	def CharWhere(fn:Function1[Char, Boolean]):Parser[Char] = new scpc.CharWhere[ContextType](fn)
	def CodepointWhere(fn:Function1[CodePoint, Boolean]):Parser[CodePoint] = new scpc.CodepointWhere[ContextType](fn)
	def IsString(str:String):Parser[Unit] = new scpc.IsString[ContextType](str)
	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType(tpe:ContextType#Type):Parser[ContextType#Tree] = new scpc.OfType[ContextType](tpe)
	/** A parser that succeeds iff the input is empty */
	def End():Parser[Unit] = new scpc.End[ContextType]()
	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[A](fn:Function0[Parser[A]]):Parser[A] = new scpc.DelayedConstruction[ContextType, A](fn)
}

private[stringContextParserCombinator] final class AndThen[U <: Context with Singleton, A, B, Z](left:Parser[U, A], right:Parser[U, B], ev:Implicits.AndThenTypes[A,B,Z]) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right.parse(resa) match {
				case Success(b, resb) => Success(ev.aggregate(a,b), resb)
				case Failure(found, expect) => Failure(found, expect)
			}
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}

private[stringContextParserCombinator] final class FlatMap[U <: Context with Singleton, A, Z](left:Parser[U, A], right:Function1[A, Parser[U, Z]]) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right(a).parse(resa)
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}

/**
 * Succeeds if the next codepoint is a member of the given function
 * @group Parser
 */
private[stringContextParserCombinator] final class CodepointWhere[U <: Context with Singleton](pred:Function1[CodePoint, Boolean]) extends Parser[U, CodePoint] {
	def parse(input:Input[U]):Result[U, CodePoint] = input.consume(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => pred(x._1)),
		_ => None,
		Failure.Leaf("???")
	)
}

/**
 * Succeeds if the next character is a member of the given Seq
 * @group Parser
 */
private[stringContextParserCombinator] final class CharIn[U <: Context with Singleton](xs:Seq[Char]) extends Parser[U, Char] {
	def parse(input:Input[U]):Result[U, Char] = input.consume(
		pt => Option((pt.charAt(0), 1)).filter(x => xs.contains(x._1)),
		_ => None,
		Failure.Or(xs.map(x => Failure.Leaf("\"" + x.toString + "\"")))
	)
}

/**
 * Succeeds if the next character is a member of the given function
 * @group Parser
 */
private[stringContextParserCombinator] final class CharWhere[U <: Context with Singleton](pred:Function1[Char, Boolean]) extends Parser[U, Char] {
	def parse(input:Input[U]):Result[U, Char] = input.consume(
		pt => Option((pt.charAt(0), 1)).filter(x => pred(x._1)),
		_ => None,
		Failure.Leaf("???")
	)
}

private[stringContextParserCombinator] final class Repeat[U <: Context with Singleton, A, Z](
	inner:Parser[U, A],
	min:Int,
	max:Int,
	ev:Implicits.RepeatTypes[A, Z]
) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		var counter:Int = 0
		val accumulator = ev.init()
		var remaining:Input[U] = input
		var continue:Boolean = true
		var innerExpecting:Failure[U] = null

		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r) => {
					counter += 1
					ev.append(accumulator, a)
					remaining = r
				}
				case failure:Failure[U] => {
					innerExpecting = failure
					continue = false
				}
			}
		}
		if (min <= counter && counter <= max) {
			return Success(ev.result(accumulator), remaining)
		} else {
			return innerExpecting
		}
	}
}

/**
 * Succeeds if the next character data is equal to the given string
 * @group Parser
 */
private[stringContextParserCombinator] final class IsString[U <: Context with Singleton](str:String) extends Parser[U, Unit] {
	def parse(input:Input[U]):Result[U, Unit] = input.consume(
		pt => Option(((), str.length())).filter(_ => pt.startsWith(str)),
		_ => None,
		Failure.Leaf("\"" + str + "\"")
	)
}

/**
 * Succeeds if the next character data matches the given Regex, and captures the matched string
 * @group Parser
 */
private[stringContextParserCombinator] final class Regex[U <: Context with Singleton](reg:scala.util.matching.Regex) extends Parser[U, String] {
	def parse(input:Input[U]):Result[U, String] = input.consume(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		_ => None,
		Failure.Leaf("s/" + reg.toString + "/")
	)
}

/**
 * Succeeds if the next input element is an `arg` with the given type
 * @group Parser
 */
private[stringContextParserCombinator] final class OfType[U <: Context with Singleton](tpe:U#Type) extends Parser[U, U#Tree] {
	def parse(input:Input[U]):Result[U, U#Tree] = input.consume(
		_ => None,
		arg => Some(arg).filter(x => x.actualType <:< tpe).map(_.tree),
		Failure.Leaf(tpe.toString)
	)
}

/**
 * Used to allow mutually recursive parsers
 * @group Parser
 */
private[stringContextParserCombinator] final class DelayedConstruction[U <: Context with Singleton, A](inner:Function0[Parser[U, A]]) extends Parser[U, A] {
	def parse(input:Input[U]):Result[U, A] = inner.apply.parse(input)
}

/**
 * Succeeds only at the end of the given input
 * @group Parser
 */
private[stringContextParserCombinator] final class End[U <: Context with Singleton] extends Parser[U, Unit] {
	def parse(input:Input[U]):Result[U, Unit] = if (input.isEmpty) {Success((), input)} else {Failure(input.next, this.expecting)}
	def expecting:Failure.Expecting = Failure.Leaf("EOF")
}
