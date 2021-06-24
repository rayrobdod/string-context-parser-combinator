---
title: Context Parameters
---

The Parser methods `andThen`, `orElse`, `repeat` and `optionally` each take a context parameter which describes how to
combine the results of the aggregate parser's components. Each of these types reside in
[[com.rayrobdod.stringContextParserCombinator.typelevel]]. Each of these four types' companion object defines one
low-priority given instance that accepts any type and produces an appropriate generic result. The companion object also
defines additional instances for more specific types, usually to avoid sticking `scala.Unit` values in a collection, and
maybe a few more for common use cases.

Defining custom instances of these types is supported. Making custom given instances can significantly reduce the number
of explicit `map` calls required when writing a parser, however the usual advice with [given
instances](https://dotty.epfl.ch/docs/reference/contextual/givens.html) applies: keep types specific, or keep the scope
of a given instance to the minimum viable to prevent given instances from becoming confusing.


# Sequenced

The fallback given sequenced places the two items in a tuple.

The `Unit`-handling sequenced values drop the unit value.

```scala
(p1:Parser[A] andThen p2:Parser[Unit]):Parser[A]
```

Below is example of providing and using custom Sequenced.

```scala
import java.time._

given (using Quotes): Sequenced[LocalDate, LocalTime, LocalDateTime] with {
	def aggregate(date:LocalDate, time:LocalTime):LocalDateTime = date.atTime(time)
}

(dateParser:Parser[LocalDate] andThen IsString("T") andThen timeParser:Parser[LocalTime]):Parser[LocalDateTime]
```


# Eithered

The fallback given Eithered creates a union type of the two component types.

The default given instances of Eithered which handle one Unit type summon a `Optionally` instance and treat the
non-Unit-typed value as a present value and the Unit-typed value as a missing value

```scala
(p1:Parser[A] orElse p2:Parser[Unit]):Parser[Option[A]]
```

A non-given instance of Eithered is provided, named `discriminatedUnion`, that instead places the two values in a
`scala.Either`.

```scala
val evenOdd:Parser[Either[Char, Char]] = CharIn("02468").orElse(CharIn("13579"))(using Eithered.discriminatedUnion)
```


# Repeated

The fallback given Sequenced places the items in a `scala.Seq`.

In addition to the given object which combines `scala.Unit` values into a single `scala.Unit`, there are two built-in
given objects which combine Char values or CodePoint values into a String.

```scala
(CharIn("abcde"):Parser[Char]).repeat():Parser[String]
```

Below is example of providing and using custom Repeated.

```scala
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
val digits:Parser[Digits] = digit.repeat(1) // using the given Repeated[Digit, Digits]
```

# Optionally

The fallback given Optionally places the items in a `scala.Option`, wrapping a found value in a Some and using a None as
the empty value.

The `Unit`-handling Optionally value doesn't wrap a present `()` in an Option, and uses `()` as a default value.

```scala
(IsString("0x"):Parser[Unit]).optionally():Parser[Unit]
```
