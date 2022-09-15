package com.rayrobdod.stringContextParserCombinatorExample

import java.net.URI

/**
 * @SeeAlso https://www.ietf.org/rfc/rfc2396.txt
 */
package object uri {
	extension (inline sc:scala.StringContext)
		inline def uri(inline args:Any*):URI =
			${MacroImpl.stringContext_uri('sc, 'args)}
}
