# string context parser combinator
[![Build status](https://ci.appveyor.com/api/projects/status/rwtfcrrc7xkjsg6s/branch/main?svg=true)](https://ci.appveyor.com/project/rayrobdod/string-context-parser-combinator)
![Run tests](https://github.com/rayrobdod/string-context-parser-combinator/workflows/Run%20tests/badge.svg)

This is a scala library for writing [custom string interpolation](https://docs.scala-lang.org/scala3/book/string-interpolation.html#advanced-usage)
implementations via parser combinators.

Alternatively, this is a parser combinator library, but in contrast to other parser combinator libraries,
this has extra leaf parsers which match the 'hole' or 'arg' parts of interpolated strings.

The maven coordinates are `"name.rayrobdod" %% "string-context-parser-combinator" % "<version>"`

The library itself is contained within the `Base/` project. The other projects are sample uses of the library.

[Documentation](https://rayrobdod.github.io/string-context-parser-combinator/SNAPSHOT/)
