A sample string context parser for URI values

```
import java.net.URI
import com.rayrobdod.stringContextParserCombinatorExample.uri._

val port = 8080
val action = "edit"

val actual = uri"http://localhost:$port/blog.php?post=hello%20world&action=$action"
val expecting = new URI("http", null, "localhost", 8080, "/blog.php", "post=hello world&action=edit", null)
assert(expecting == actual)
```
