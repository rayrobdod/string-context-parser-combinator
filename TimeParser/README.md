Sample string context parsers for creating `java.date.LocalDate`, `java.date.LocalTime` and `java.date.LocalDateTime`
values.

```scala
import java.time._
import name.rayrobdod.stringContextParserCombinatorExample.datetime._

val month = Month.FEBRUARY

assert(LocalDate.of(2001, 2, 3) == localdate"2001-$month-03")
assert(LocalTime.of(4, 5, 6, 700_000_000) == localtime"04:05:06.7")
assert(LocalDateTime.of(2001, 2, 3, 4, 5, 6, 0) == localdatetime"2001-$month-03T04:05:06")
```

This implementation demonstrates having a parser that makes heavy use of custom typeclass implementations,
and that uses a layer of indirection to allow cross-built code to share some of the parser implementation.

As with the other examples, the extension method is defined in `src/main/scala-(2|3)/package.scala`,
however in this case the bulk of the macro implementation is in `src/main/scala/TimeParsers.scala`, in the shared sources.
