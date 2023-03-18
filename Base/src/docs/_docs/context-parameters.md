---
title: Context Parameters
---

The [[Parser|com.rayrobdod.stringContextParserCombinator.Parser]] methods
`andThen`, `orElse`, `repeat` and `optionally` each take a context parameter which describes how to
combine the results of the aggregate parser's components. Each of these types reside in
[[com.rayrobdod.stringContextParserCombinator.typeclass]]. Each of these four types' companion object defines one
low-priority given instance that accepts any type and produces an appropriate generic result. The companion object also
defines additional instances for more specific types, usually to avoid sticking `scala.Unit` values in a collection, and
maybe a few more for common use cases.

Defining custom instances of these types is supported. Making custom given instances can significantly reduce the number
of explicit `map` calls required when writing a parser, however the usual advice with [given
instances](https://docs.scala-lang.org/scala3/reference/contextual/givens.html) applies: keep types specific, or keep the scope
of a given instance to the minimum viable to prevent given instances from becoming confusing.


## Sequenced

A [[Sequenced|com.rayrobdod.stringContextParserCombinator.typeclass.Sequenced]] describes how to combine two adjacent values into
one value.

The fallback given sequenced places the two items in a tuple.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
class A {}
class B {}
val p1:Parser[A] = ???
val p2:Parser[B] = ???

//}
((p1:Parser[A]) andThen (p2:Parser[B])):Parser[(A, B)]
```

This library provides `Sequenced` values that handle the case where one value or the other is a `Unit`. When these are
used, the unit value is dropped, leaving the other value in tact. If both values are `Unit`, then the result is `Unit`.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
class A {}
val u1:Parser[Unit] = ???
val u2:Parser[Unit] = ???
val p1:Parser[A] = ???

//}
((u1:Parser[Unit]) andThen (u2:Parser[Unit])):Parser[Unit]
((p1:Parser[A]   ) andThen (u2:Parser[Unit])):Parser[A]
((u1:Parser[Unit]) andThen (p1:Parser[A]   )):Parser[A]
```

The Sequenced interface consists of a single method which takes the two values as parameters and returns the resulting value.

Below is example of defining and using a custom Sequenced.

```scala
import java.time._
//{
import com.rayrobdod.stringContextParserCombinator.Parser._
import com.rayrobdod.stringContextParserCombinator.typeclass.Sequenced
val dateParser:Parser[LocalDate] = ???
val timeParser:Parser[LocalTime] = ???

//}
given Sequenced[LocalDate, LocalTime, LocalDateTime] with {
	def aggregate(date:LocalDate, time:LocalTime):LocalDateTime = date.atTime(time)
}

((dateParser:Parser[LocalDate]) andThen IsString("T") andThen (timeParser:Parser[LocalTime])):Parser[LocalDateTime]
```


## Eithered

An [[Eithered|com.rayrobdod.stringContextParserCombinator.typeclass.Eithered]] describes how to treat a parser result
that may be the result of one parser or the result of the other parser.

The fallback given Eithered creates a union type of the two component types. Since the union of a type with itself is
equivalent to that same type, if this fallback Eithered is used for two parsers of the same type, then the result is a
parser of that type.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
import com.rayrobdod.stringContextParserCombinator.typeclass.Eithered
class A {}
class B {}
val p1:Parser[A] = ???
val p2:Parser[B] = ???
val p3:Parser[A] = ???

//}
((p1:Parser[A]) orElse (p2:Parser[B])):Parser[A | B]
((p1:Parser[A]) orElse (p3:Parser[A])):Parser[A]
```

This library provides given `Eithered` values that handle the case where one value or the other is a `Unit`. If both options
are Parsers of Unit, the result is a Parser of Unit. Otherwise the Unit case is converted to a `scala.None`, and the
non-unit case is wrapped in a `scala.Some`.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
class A {}
val p1:Parser[Unit] = ???
val p2:Parser[Unit] = ???
val p3:Parser[A] = ???

//}
((p1:Parser[Unit]) orElse (p2:Parser[Unit])):Parser[Unit]
((p3:Parser[A]   ) orElse (p2:Parser[Unit])):Parser[Option[A]]
```

This library provides a non-given instance of Eithered, named `discriminatedUnion`, that instead places the two values
in a `scala.Either`.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser._
import com.rayrobdod.stringContextParserCombinator.typeclass.Eithered
class A {}
class B {}
val p1:Parser[A] = ???
val p2:Parser[B] = ???

//}
val discriminated:Parser[Either[A, B]] = (p1:Parser[A]).orElse(p2:Parser[B])(using Eithered.discriminatedUnion)
// in the following, even digits are placed in a Left while odd digits are placed in a Right.
val evenOdd:Parser[Either[Char, Char]] = CharIn("02468").orElse(CharIn("13579"))(using Eithered.discriminatedUnion)
```

The interface consists of two methods, the `left` method called if the first choice succeeded, or `right` if the second
choice succeeded.

Below is example of defining and using a custom Eithered.

```scala
import java.io.File
import java.net.URI
import java.util.UUID
//{
import com.rayrobdod.stringContextParserCombinator.Parser._
import com.rayrobdod.stringContextParserCombinator.typeclass.Eithered
val uuidParser:Parser[UUID] = ???
val fileParser:Parser[File] = ???

//}
given Eithered[File, UUID, URI] with {
	def left(f:File):URI = f.toURI
	def right(id:UUID):URI = new URI("urn", s"uuid:${id}", null)
}

((fileParser:Parser[File]) orElse (uuidParser:Parser[UUID])):Parser[URI]
```


## Repeated

A [[Repeated|com.rayrobdod.stringContextParserCombinator.typeclass.Repeated]] describes how to combine a homogeneous
sequence of zero-or-more values.

The fallback given Repeated places the items in a `scala.Seq`.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
class A {}
val p1:Parser[A] = ???

//}
((p1:Parser[A]).repeat()):Parser[List[A]]
```

A given Repeated is provided which combines repeated `scala.Unit` values into a single `scala.Unit`

A given Repeated is provided which combines repeated `scala.Char` values into a `String`, and another that combines
repeated `CodePoint` values into a `String`.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.CodePoint
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
val unitParser:Parser[Unit] = ???
val charParser:Parser[Char] = ???
val codePointParser:Parser[CodePoint] = ???

//}
(unitParser:Parser[Unit]).repeat():Parser[Unit]
(charParser:Parser[Char]).repeat():Parser[String]
(codePointParser:Parser[CodePoint]).repeat():Parser[String]
```

When a Repeated is used
 * first, `init` is called to create a new mutable builder
 * then, `append` is called once for each component item in order, using the `init`-created builder and the component item as parameters
 * lastly, `result` is called with the builder, and the result of this call is overall result.

Below is example of providing and using custom Repeated.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser._
import com.rayrobdod.stringContextParserCombinator.typeclass.Repeated

//}
// define the marker types
/** represents a single digit */
case class Digit(value:Int)
/** represents a sequence of digits */
case class Digits(value:Int)

// define the given instance
given Repeated[Digit, Digits] with {
	final class Box(var value:Int)
	type Acc = Box
	def init():Acc = new Box(0)
	def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
	def result(acc:Acc):Digits = new Digits(acc.value)
}

// create the parsers
val digit:Parser[Digit] = CharIn('0' to '9').map(x => Digit(x - '0'))
val digits:Parser[Digits] = digit.repeat(1)
```

## Optionally

An [[Optionally|com.rayrobdod.stringContextParserCombinator.typeclass.Optionally]] describes the result of a parser that
might parse another value.

The fallback given Optionally places the items in a `scala.Option`, wrapping a found value in a Some and using a None as
the empty value.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
class A {}
val p1:Parser[A] = ???

//}
((p1:Parser[A]).optionally()):Parser[Option[A]]
```

The `Unit`-handling Optionally value doesn't wrap a present `()` in an Option, and uses `()` as a default value.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser.Parser
val p1:Parser[Unit] = ???

//}
((p1:Parser[Unit]).optionally()):Parser[Unit]
```

The interface consists of two methods, the `some` method called if there was a value, and `none` method called if there
was no value.

Below is example of defining and using a custom Optionally. This Optionally has the effect of using the empty string as
a default value.

```scala
//{
import com.rayrobdod.stringContextParserCombinator.Parser._
import com.rayrobdod.stringContextParserCombinator.typeclass.Optionally
val stringParser:Parser[String] = ???

//}
given Optionally[String, String] with {
	def none:String = ""
	def some(s:String):String = s
}

((stringParser:Parser[String]).optionally()):Parser[String]
```
