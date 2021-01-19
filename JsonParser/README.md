This sample provides a `json` interpolator, that creates JValues from the interpolated string.

```scala
import org.json4s.JsonAST._
import com.rayrobdod.stringContextParserCombinatorExample.json._

val foo:Int = 123
val bar:String = "abc"

val result:JValue = json"""{
	"foo": $foo,
	"bar": $bar
}"""
val expecting:JValue = JObject(List("foo" -> JLong(foo), "bar" -> JString(bar)))
assert(expecting == result)
```
