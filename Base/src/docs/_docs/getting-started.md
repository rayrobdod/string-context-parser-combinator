---
title: Getting Started
---

For our first parser, we'll implement a string interpolator equivalent to the standard `s` interpolator.
That is, an interpolator that builds a string consisting of the concatenation of literal parts and arguments converted to strings using `toString`.

The crux of parser combinators is creating a parser by combining smaller parsers together,
so we'll start with a small parser and build up to a parser with the desired properties.

The leaf parsers that we'll be using for now are provided in [Interpolator.idInterpolators](name.rayrobdod.stringContextParserCombinator.Interpolator$.idInterpolators).
And we're [defining a new string interpolation](https://docs.scala-lang.org/scala3/book/string-interpolation.html#advanced-usage).
So, the scaffolding will look something like:

```scala
//{
type Result
//}
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

extension (sc:StringContext)
  def prefix(args:Any*):Result =
    val interpolator:Interpolator[Result] = ???
    interpolator.interpolate(sc, args)
```

We'll start by creating a Interpolator that will match one of any character from the processed string.
[[charWhere|name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolators.charWhere]] is one of the leaf parsers that can be used for this;
`charWhere` takes a predicate and creates a parser in which,
if the next character passes the predicate, the parser passes and the character is captured.
Since we want this parser to match any character,
we will use a predicate that always returns true.

```scala
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

extension (sc:StringContext)
  def s2(args:Any*):Char =
    val anyChar = charWhere(_ => true)
    anyChar.interpolate(sc, args)

s2"Hello" // 'H'
s2"${1 + 1}" // throws
s2"" // throws
```

Note that the parser matches the first character if there is one. Parsing starts at the start of the processed string.

Using this parser that can match one of any character,
we can create a parser that can match a sequence of characters using the [[repeat|name.rayrobdod.stringContextParserCombinator.Interpolator.repeat]] operator.
The `repeat` operator creates a parser that will invoke the operand repeatedly until the operand parser fails,
and then combine the results of the repeated operand runs into a new value.
Using the default givens, since the operand is a `Interpolator[Char]`, the result is a `Interpolator[String]`.

```scala
//{
import _root_.name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):String =
    val anyChars = charWhere(_ => true)
        .repeat()
    anyChars.interpolate(sc, args)

s2"Hello" // "Hello"
val name = "Mr. Smith"
s2"Hello ${name}!" // "Hello "
s2"" // ""
```

Next, lets handle processed string arguments.
We will set aside `anyChars` for now.
Of the leaf parsers that handle args, [[ofType|name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolators.ofType]] is the most straightforward.
`ofType` takes a type argument and type evidence and will match and capture any argument that is a subtype of that class.
So, an `ofType[Int]` would match any argument that is an `Int` or a subclass of `Int`.
Since we want to match any argument, we will use `ofType[Any]`.
The result of running this parser is the same as its type parameter, in this case `Any`.

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):Any =
    val anyArg = ofType[Any]
    anyArg.interpolate(sc, args)

s2"${2 + 2}" // 4
s2"Hello" // throws: Expected ofType[Object]
```

We don't want the result of the interpolator to be an Any here, though; we want the result to be a String.

We can use the [[map|name.rayrobdod.stringContextParserCombinator.Interpolator.map]] operator to convert an interpolator on one type to an interpolator on another type.

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):String =
    val anyArg = ofType[Any]
        .map(_.toString)
    anyArg.interpolate(sc, args)

s2"${2 + 2}" // "4"
s2"Hello" // throws: Expected ofType[Object]
```

Now that we have one parser that will match a sequence of characters and another that will match an arg,
we can create a parser that will match either a sequence of characters or an arg by combing the two other parsers using the [[orElse|name.rayrobdod.stringContextParserCombinator.Interpolator.orElse]] operator.
The `orElse` operator creates a parser that will attempt the left parser,
passing the result of left parser if the result was a success,
otherwise attempting the right parser and passing that result.

Using the default givens, since both arguments to the orElse operator are `Interpolator[String]`,
the result of the operator will also be a `Interpolator[String]`

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):String =
    val anyChars = charWhere(_ => true)
        .repeat()
    val anyArg = ofType[Any]
        .map(_.toString)
    val segment = anyChars orElse anyArg
    segment.interpolate(sc, args)

s2"2 + 2 = ${2 + 2}" // "2 + 2 = "
s2"${2 + 2}" // ""
```

Oh, the parser didn't do quite what we wanted.
Here, the parser saw that the processed string started with zero characters and considered that to be a match of the `anyChars` branch.
To fix this, we are going to modify the `repeat` call in `anyChars`.
`repeat` has several optional arguments,
the first of which is the minimum number of repeats required for the parse to be considered a success.
This argument defaults to zero, but if it is explicitly set to one and the processed string starts with an arg,
then `anyChars` will not consider a run of zero characters to be a success,
and `segment` will try the `anyArg` branch after the `anyChars` branch fails.

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):String =
    val anyChars = charWhere(_ => true)
        .repeat(1)
    val anyArg = ofType[Any]
        .map(_.toString)
    val segment = anyChars orElse anyArg
    segment.interpolate(sc, args)

s2"2 + 2 = ${2 + 2}" // "2 + 2 = "
s2"${2 + 2} = 2 + 2" // "4"
```

Now that we have a parser that can match either a run of characters or an argument,
we can `repeat` that parser to create a parser that can match a sequence of character-sequences-or-arguments.
This time, since the input to the repeat parser is a `Interpolator[String]`, the result will be a `Interpolator[List[String]]`.
In general, unless a higher priority instance of the Repeated typeclass can be found, `repeat` will create a parser that produces a `List[A]`;
the `Char` to `String` seen before was a built-in higher priority Repeated typeclass instance.

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):List[String] =
    val anyChars = charWhere(_ => true)
        .repeat(1)
    val anyArg = ofType[Any]
        .map(_.toString)
    val segment = anyChars orElse anyArg
    val segments = segment
        .repeat()
    segments.interpolate(sc, args)

s2"2 + 2 = ${2 + 2}" // List("2 + 2 = ", "4")
s2"${2 + 2} = 2 + 2" // List("4", " = 2 + 2")
```

We have a `Interpolator[Seq[String]]`, and we can map a `Seq[String]` to an `String` to finish the simple string context reimplementation.

```scala
//{
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

//}
extension (sc:StringContext)
  def s2(args:Any*):String =
    val anyChars = charWhere(_ => true)
        .repeat(1)
    val anyArg = ofType[Any]
        .map(_.toString)
    val segment = anyChars orElse anyArg
    val segments = segment
        .repeat()
        .map(_.mkString)
    segments.interpolate(sc, args)

s2"2 + 2 = ${2 + 2}" // "2 + 2 = 4"
```

This interpolator does work, however this interpolator works at run time.

The library also supports creating marco-level parsers, that will instead run at compile time.
There are several advantages to using a macro-based interpolator,

* Parsing errors will fail at compile time, instead of being a runtime exception
* `ofType` can work with types instead of classes,
* The `Lifted` interpolator only works in the Quoted context

The leaf interpolators used for a macro-based interpolator at provided in [[Interpolator.quotedInterpolators|name.rayrobdod.stringContextParserCombinator.Interpolator$.quotedInterpolators]]
(or equivalently provided directly in the [[`Interpolator` companion object|name.rayrobdod.stringContextParserCombinator.Interpolator$]]).
The extension method declaration changes to that of a [[macro definition|https://docs.scala-lang.org/scala3/guides/macros/macros.html]].

Together the scaffolding of the string context extension method becomes

```scala
type Result
import scala.quoted.{Expr, Quotes}
import name.rayrobdod.stringContextParserCombinator.Interpolator._

extension (inline sc:StringContext)
  inline def prefix(inline args:Any*):Result =
    ${prefixImpl('sc, 'args)}

def prefixImpl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[Result] =
  val interpolator:Interpolator[Expr[Result]] = ???
  interpolator.interpolate(sc, args)
```

The `interpolate` method handles extracting string context parts and arguments from the Expr arguments.
Most of the changes involve changing return values to be wrapped in an Expr.

The `charWhere` and the other character parsers capture `Char` values even in macro interpolators.
However, the result must be wrapped in an `Expr`,
so the parts must must be lifted into an Expr at some point.
This can be done with the map operator, such as `.map(Expr(_))`, or equivalently with the [[mapToExpr|name.rayrobdod.stringContextParserCombinator.Interpolator.mapToExpr]] method.

```diff
 val anyChars = charWhere(_ => true)
     .repeat(1)
+    .mapToExpr
```

`ofType` changes from requiring an implicit `scala.reflect.ClassTag` and returning an unwrapped value,
to requiring an implicit `scala.quoted.Type` and returning an `Expr` that builds a value.
Since the `ofType` result is in an `Expr`, the mapping applied to this value must be changed from a `Any => String` to a `Expr[Any] => Expr[String]`, essentially wrapping the mapping in a Quote.

```diff
 val anyArg = ofType[Any]
-    .map(arg => arg.toString)
+    .map(arg => '{$arg.toString})
```

The operands that create a segment have changed from both being a `Interpolator[String]` to both being an `Interpolator[Expr[String]]`,
so the `orElse`-combination of the two parts changes in the same way, but no source changes occur as a consequence of this.

```diff
 val segment = anyChars orElse anyArg
```

Lastly, the `segments` mapping has to be changed from a `Seq[String] => String` to a `Seq[Expr[String]] => Expr[String]`.

```diff
 val segments = segment
     .repeat()
-    .map(_.mkString)
+    .map(strExprs => '{${Expr.ofList(strExprs)}.mkString})
```

Taken together, the macro-level reimplementation of the standard string interpolator is as follows:

```scala
import scala.quoted.{Expr, Quotes}
import name.rayrobdod.stringContextParserCombinator.Interpolator._

extension (inline sc:StringContext)
  inline def s2(inline args:Any*):String =
    ${s2Impl('sc, 'args)}

def s2Impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] =
  val anyChars = charWhere(_ => true)
      .repeat(1)
      .mapToExpr

  val anyArg = ofType[Any]
      .map(arg => '{$arg.toString})

  val segment = anyChars orElse anyArg

  val segments = segment
      .repeat()
      .map(strExprs => '{${Expr.ofList(strExprs)}.mkString})

  segments.interpolate(sc, args)
```
