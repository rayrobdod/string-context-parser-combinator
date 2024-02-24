package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import Interpolator.idInterpolators.{charWhere, ofType, end}

final class IdConcatenateString extends BaseInterpolatorSuite {
	val dut = (charWhere(_ => true).repeat(1) orElse ofType[String])
		.repeat()(using Repeated.idConcatenateString)
		.andThen(end)

	test ("0") {
		assertParseSuccess(dut, ("" :: Nil, Nil), "")
	}
	test ("1") {
		assertParseSuccess(dut, ("ab" :: Nil, Nil), "ab")
	}
	test ("many") {
		assertParseSuccess(dut, ("ab" :: "cd" :: "ef" :: Nil, "12" :: "34" :: Nil), "ab12cd34ef")
	}
}
