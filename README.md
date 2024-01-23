# string context parser combinator
[![Build status](https://ci.appveyor.com/api/projects/status/rwtfcrrc7xkjsg6s/branch/main?svg=true)](https://ci.appveyor.com/project/rayrobdod/string-context-parser-combinator)
[![Check](https://github.com/rayrobdod/string-context-parser-combinator/actions/workflows/run-tests.yml/badge.svg)](https://github.com/rayrobdod/string-context-parser-combinator/actions/workflows/run-tests.yml)

This is a scala library for writing [custom string interpolation](https://docs.scala-lang.org/scala3/book/string-interpolation.html#advanced-usage)
implementations via parser combinators.

The parser combinators, in addition to parser leaves that match on parts of a string, such as `charWhere`,
include leaves that match on the 'hole' or 'arg' parts of interpolated strings.
As such, this can be used to create compile-time checked string interpolators that accept holes;
contrast [literally](https://github.com/typelevel/literally/), which cannot accept holes,
or using `s` to turn the StringContext into a string then parsing the built string, which cannot be checked at compile-time.
The parser can do this while still being easier and faster to create than a hand-rolled parser.

The maven coordinates are `"name.rayrobdod" %% "string-context-parser-combinator" % "<version>"`

The library itself is contained within the `Base/` project. The other projects are sample uses of the library.

[Documentation](https://rayrobdod.github.io/string-context-parser-combinator/SNAPSHOT/)
