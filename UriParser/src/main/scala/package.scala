package com.rayrobdod.stringContextParserCombinator.example

import java.net.URI
import scala.language.experimental.macros

/**
 * @SeeAlso https://www.ietf.org/rfc/rfc2396.txt
 */
package object uri {
	implicit final class UriStringContext(val backing:StringContext) extends AnyVal {
		def uri(args:Any*):URI = macro MacroImpl.stringContext_uri
	}
}
