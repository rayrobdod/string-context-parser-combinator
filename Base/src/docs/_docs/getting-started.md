---
title: Getting Started
---

For our first parser, we'll implement a string interpolator equivalent to the standard `s` interpolator. To start with,
we'll create a `s2` StringContext extension, same as we would for any other StringContext extension macro, and the
scaffold for a method implementing this macro.

```scala sc:nocompile
extension (inline sc:StringContext)
  inline def s2(inline args:Any*):String =
    ${s2impl('sc, 'args)}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = ???
```

From here on, every change will be in the macro impl function; we'll iterate on s2impl until we get our final parser.

We'll start by creating a Parser that will match one of any character from the processed string. `CharWhere` is one of
the leaf parsers that can be used for this; `CharWhere` takes a predicate and a description of the predicate, used for
error reporting; if the next character passes the predicate, then the CharWhere parser matches the character and
captures the character. Since we want this parser to match any character, we will use a predicate that always returns
true.

The result of this parser will be a `Char`, but since we know that we want our macro to return a `Expr[String]` (and
since it has to return an `Expr[_]` since it is a macro), we will force the result into this shape to test the macro.

```scala
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)

  val result = anyChar.interpolate(sc, args)
  // `result` is a `Char`. Stringify the result and wrap it to fit the required shape.
  Expr(s"$result")
}
```

```
s2"Hello" == "H"
s2"${name}" fails with Expected anyChar
```

Note that the parser matches the first character if there is one. Parsing starts at the start of the processed string.

Using a parser that can match one of any character, we can create a parser that can match a sequence of characters using
the `repeat` operator. The repeat operator creates a parser that will invoke the operand repeatedly until the operand
parser fails, and then combine the results of the repeated operand runs into a new value. In this case, since the
operand is a `Interpolator[Char]`, the result is a `Interpolator[String]`.

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)
  val anyChars = anyChar.repeat()

  val result = anyChars.interpolate(sc, args)
  // `result` is a `String`. wrap it to fit the required shape.
  Expr(s"$result")
}
```

```
s2"Hello." == "Hello."
s2"Hello ${name}!" == "Hello "
```

Now, we'd like our parser to directly return an `Expr[String]`, instead of returning whatever and requiring us to
transform it outside of the parser. Especially since after this, we'll be dealing with args that are already `Expr`s.

The map operator takes a `Interpolator[A]` and a mapping `A => B` and will create a parser which will consume the same input
an the input parser, but will return the result of applying the mapping to the input parser's result. In this case, we
want `anyChars` to return a `Expr[String]` instead of a `String`, so we will apply a mapping of `String => Expr[String]`
to the parser.

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)
  val anyChars = anyChar.repeat(1).map(str => Expr(str))

  anyChars.interpolate(sc, args)
}
```

This version is functionally equivalent to the previous version.

Next, lets handle processed string arguments. We will set aside `anyChars` for now. Of the leaf parsers that handle
args, `OfType` is the simplest to use. An `OfType` takes a type parameter and will match any argument of that type and
return that argument. So, an `OfType[Int]` would match any argument that is an `Expr[Int]` or a subclass of Int. Since
we want to match any Expr, we will use `OfType[Any]`. The result of running this parser is `Expr[Any]`, but we want an
`Expr[String]`, so we will map the parser using the `Any.toString` method.

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyArg = OfType[Any].map(anyExpr => '{$anyExpr.toString()})

  anyArg.interpolate(sc, args)
}
```

```
s2"${name}" == "Mr. Smith"
s2"Hello ${name}!" fails with Expected OfType[Any]
```

Now that we have one parser that will match a sequence of characters and another that will match an arg, we can create a
parser that will match either a sequence of characters or an arg by combing the two other parsers using the `orElse`
operator. The `orElse` operator creates a parser that will attempt the left parser, passing the result of left parser if
the result was a success, otherwise attempting the right parser and passing that result. Since both arguments to the
orElse operator are `Interpolator[Expr[String]]`, the result of the operator will also be a `Interpolator[Expr[String]]`

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)
  val anyChars = anyChar.repeat().map(str => Expr(str))
  val anyArg = OfType[Any].map(anyExpr => '{$anyExpr.toString()})
  val segment = anyChars orElse anyArg

  segment.interpolate(sc, args)
}
```

```
s2"Hello ${name}!" == "Hello "
s2"${name}" == ""
```

Oh, the parser didn't do quite what we wanted. Here, the parser saw that the string started with zero characters and
considered that to be a match of the `anyChars` branch. To fix this, we are going to modify the `repeat` call in
`anyChars`. `repeat` has several optional arguments, the first of which is the minimum number of repeats required for
the parse to be considered a success. This argument defaults to zero, but if it is explicitly set to one and the
processed string starts with an arg, then `anyChars` will not consider a run of zero characters to be a success, and
`segment` will try the `anyArg` branch after the `anyChars` branch fails.

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)
  val anyChars = anyChar.repeat(1).map(str => Expr(str))
  val anyArg = OfType[Any].map(anyExpr => '{$anyExpr.toString()})
  val segment = anyChars orElse anyArg

  segment.interpolate(sc, args)
}
```

```
s2"Hello ${name}!" == "Hello "
s2"${name}" == "Mr. Smith"
```

Now that we have a parser that can match either a run of characters or an argument, we can `repeat` that parser to
create a parser that can match a sequence of character-sequences-or-arguments. Similar to last time, however, since the
input to the repeat parser is a `Interpolator[Expr[String]]`, the result will instead be a `Interpolator[Seq[Expr[String]]]`. In
general, without providing a custom instance of the Repeated typeclass, `repeat` will create a parser that produces a
`Seq[A]`; the `Char` to `String` seen before was a special case. Anyway, we have a `Interpolator[Seq[Expr[String]]]`, and we
can map a `Seq[Expr[String]]` to an `Expr[String]`, using that mapping will give us our final parser.

```scala
//{
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Interpolator._
//}

def s2impl(sc:Expr[StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[String] = {
  val anyChar = CharWhere(_ => true)
  val anyChars = anyChar.repeat(1).map(str => Expr(str))
  val anyArg = OfType[Any].map(anyExpr => '{$anyExpr.toString()})
  val segment = anyChars orElse anyArg
  val segments = segment.repeat().map(strExprs => '{${Expr.ofList(strExprs)}.mkString})
  // concatenation using a StringBuilder instead of List::mkString is left as an exercise for the reader

  segment.interpolate(sc, args)
}
```

```
s2"Hello ${name}!" == "Hello Mr. Smith!"
s2"1 + 1 = ${1 + 1}" == "1 + 1 = 2"
```

Now, we have a re-implementation of the standard string interpolator.
