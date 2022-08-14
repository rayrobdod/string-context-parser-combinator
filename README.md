# string context parser combinator
[![Build status](https://ci.appveyor.com/api/projects/status/rwtfcrrc7xkjsg6s/branch/main?svg=true)](https://ci.appveyor.com/project/rayrobdod/string-context-parser-combinator)
![Run tests](https://github.com/rayrobdod/string-context-parser-combinator/workflows/Run%20tests/badge.svg)

This is a library for making parsers for String Context extension implementations.

Basically, this is a parser combinator library, but compared to other string context libraries, it has extra leaf
parsers which match the 'hole' or 'arg' parts of interpolated strings and is otherwise designed to be used in
scala macros.

The library is contained within the `Base/` project. The other projects are sample uses of the library.
