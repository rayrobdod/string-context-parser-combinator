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
