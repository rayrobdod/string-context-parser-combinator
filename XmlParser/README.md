This sample provides a `xml` interpolator.

Includes delegation to a name based "Factory" to tell the parser how to create elements, attributes, etc.,
and uses of the low-level reflect api to call those factory methods

```scala
import scala.xml.*
import name.rayrobdod.stringContextParserCombinatorExample.xml._

val attrValue = Text("value")
val innerNode = Elem("ns1", "xyz", Null, TopScope, true)
val result = xml"<ns1:abc other='data' attr=${attrValue}>textdata${innerNode}</ns1:abc>"

val expecting = Elem("ns1", "abc", UnprefixedAttribute("other", "data", UnprefixedAttribute("attr", "value", Null)), TopScope, true, Text("textdata"), innerNode)

assert(expecting == result)
```
