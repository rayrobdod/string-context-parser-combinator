package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.reflect.macros.blackbox.Context

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
	def filter(fn:Function1[A, Boolean]):Parser[U, A] = new Parser[U, A] {
		def parse(input:Input[U]):Result[U, A] = Parser.this.parse(input) match {
			case Success(v, r) if fn(v) => Success(v, r)
			case Success(_, _) => Failure(Seq("???"))
			case Failure(ex) => Failure(ex)
		}
	}

	def opaque(description:String):Parser[U, A] = new Parser[U, A] {
		def parse(input:Input[U]):Result[U, A] = Parser.this.parse(input).orElse(Failure(Seq(description)))
	}

	def andThen[B, Z](rhs:Parser[U, B])(implicit ev:Implicits.AndThenTypes[A,B,Z]):Parser[U, Z] = {
		new AndThen(this, rhs, ev)
	}
	def orElse[Z >: A](rhs:Parser[U, Z]):Parser[U, Z] = new Parser[U, Z] {
		def parse(input:Input[U]):Result[U, Z] = Parser.this.parse(input).orElse(rhs.parse(input))
	}
	def repeat[Z](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[A, Z]):Parser[U, Z] = new Repeat(this, min, max, ev)
}

private[stringContextParserCombinator] final class AndThen[U <: Context with Singleton, A, B, Z](left:Parser[U, A], right:Parser[U, B], ev:Implicits.AndThenTypes[A,B,Z]) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right.parse(resa) match {
				case Success(b, resb) => Success(ev.aggregate(a,b), resb)
				case Failure(ex) => Failure(ex)
			}
			case Failure(ex) => Failure(ex)
		}
	}
}

private[stringContextParserCombinator] final class FlatMap[U <: Context with Singleton, A, Z](left:Parser[U, A], right:Function1[A, Parser[U, Z]]) extends Parser[U, Z] {
	def parse(input:Input[U]):Result[U, Z] = {
		left.parse(input) match {
			case Success(a, resa) => right(a).parse(resa)
			case Failure(ex) => Failure(ex)
		}
	}
}

/**
 * Succeeds if the next codepoint is a member of the given function
 * @group Parser
 */
final class CodepointWhere[U <: Context with Singleton](pred:Function1[CodePoint, Boolean]) extends Parser[U, CodePoint] {
	def parse(input:Input[U]):Result[U, CodePoint] = input.consume(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => pred(x._1)),
		_ => None,
		Seq("???")
	)
}

/**
 * Succeeds if the next character is a member of the given Seq
 * @group Parser
 */
final class CharIn[U <: Context with Singleton](xs:Seq[Char]) extends Parser[U, Char] {
	def parse(input:Input[U]):Result[U, Char] = input.consume(
		pt => Option((pt.charAt(0), 1)).filter(x => xs.contains(x._1)),
		_ => None,
		xs.map(x => s""""$x"""")
	)
}

/**
 * Succeeds if the next character is a member of the given function
 * @group Parser
 */
final class CharWhere[U <: Context with Singleton](pred:Function1[Char, Boolean]) extends Parser[U, Char] {
	def parse(input:Input[U]):Result[U, Char] = input.consume(
		pt => Option((pt.charAt(0), 1)).filter(x => pred(x._1)),
		_ => None,
		Seq("???")
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
		var innerExpecting:Seq[String] = Seq.empty

		while (continue && counter < max) {
			inner.parse(remaining) match {
				case Success(a, r) => {
					counter += 1
					ev.append(accumulator, a)
					remaining = r
				}
				case Failure(ex) => {
					innerExpecting = ex
					continue = false
				}
			}
		}
		if (min <= counter && counter <= max) {
			return Success(ev.result(accumulator), remaining)
		} else {
			return Failure(Seq(expectingString(innerExpecting)))
		}
	}
	def expectingString(innerExpecting:Seq[String]):String = {
		val prefix = {
			if (min == 0 && max == Integer.MAX_VALUE) {"Zero or more"} else
			if (min == 0 && max == 1) {"Zero or one"} else
			if (min == 1 && max == Integer.MAX_VALUE) {"One or more"} else
			if (max == Integer.MAX_VALUE) {s"$min or more"} else
			s"Between $min and $max"
		}
		val suffix = innerExpecting match {
			case Seq() => "???"
			case Seq(x) => x
			case _ => innerExpecting.mkString("<", ", ", ">")
		}
		s"$prefix copies of $suffix"
	}
}

/**
 * Succeeds if the next character data is equal to the given string
 * @group Parser
 */
final class IsString[U <: Context with Singleton](str:String) extends Parser[U, Unit] {
	def parse(input:Input[U]):Result[U, Unit] = input.consume(
		pt => Option(((), str.length())).filter(_ => pt.startsWith(str)),
		_ => None,
		Seq(s""""$str"""")
	)
}

/**
 * Succeeds if the next character data matches the given Regex, and captures the matched string
 * @group Parser
 */
final class Regex[U <: Context with Singleton](reg:scala.util.matching.Regex) extends Parser[U, String] {
	def parse(input:Input[U]):Result[U, String] = input.consume(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		_ => None,
		Seq(s"s/${reg}/")
	)
}

/**
 * Succeeds if the next input element is an `arg` with the given type
 * @group Parser
 */
final class OfType[U <: Context with Singleton](tpe:U#Type) extends Parser[U, U#Tree] {
	def parse(input:Input[U]):Result[U, U#Tree] = input.consume(
		_ => None,
		arg => Some(arg).filter(x => x.actualType <:< tpe).map(_.tree),
		Seq(tpe.toString)
	)
}

/**
 * Used to allow mutually recursive parsers
 * @group Parser
 */
final class DelayedConstruction[U <: Context with Singleton, A](inner:Function0[Parser[U, A]]) extends Parser[U, A] {
	def parse(input:Input[U]):Result[U, A] = inner.apply.parse(input)
}

/**
 * Succeeds only at the end of the given input
 * @group Parser
 */
final class End[U <: Context with Singleton] extends Parser[U, Unit] {
	def parse(input:Input[U]):Result[U, Unit] = input.checkEmpty
}
