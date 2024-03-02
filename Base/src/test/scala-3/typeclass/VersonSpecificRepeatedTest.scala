package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import scala.quoted.*
import munit.Location

final class QuotedConcatenateStringTest extends munit.FunSuite {
	inline def assertParseSuccess(
		inline sc: StringContext,
		inline args: Any*)(
		expecting:String)(
		using loc:Location
	):Unit = ${
		QuotedConcatenateStringTestImpls.assertParseSuccessImpl(
			'this, 'sc, 'args, 'expecting, 'loc)
	}

	test ("0") {
		assertParseSuccess(StringContext(""))("")
	}
	test ("1") {
		assertParseSuccess(StringContext("ab"))("ab")
	}
	test ("many") {
		assertParseSuccess(StringContext("ab", "cd", "ef"), "12", "34")("ab12cd34ef")
	}
}
