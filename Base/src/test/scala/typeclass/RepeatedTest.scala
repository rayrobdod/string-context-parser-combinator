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

final class IdFromSplicesToList extends BaseInterpolatorSuite {
	import Repeated.SplicePiece
	val dut = (ofType[SplicePiece[Id, Int]])
		.repeat()(using Repeated.idFromSplicesToList)
		.andThen(end)

	test ("Nil") {
		assertParseSuccess(dut, ("" :: Nil, Nil), Nil)
	}
	test ("Zero") {
		assertParseSuccess(dut, ("" :: "" :: Nil, SplicePiece.Zero[Id]() :: Nil), Nil)
	}
	test ("Scalar") {
		assertParseSuccess(dut, ("" :: "" :: Nil, SplicePiece.One[Id, Int](42) :: Nil), 42 :: Nil)
	}
	test ("Splice") {
		assertParseSuccess(dut, ("" :: "" :: Nil, SplicePiece.Many[Id, Int](3 to 6) :: Nil), 3 :: 4 :: 5 :: 6 :: Nil)
	}
	test ("Mix") {
		assertParseSuccess(dut, ("" :: "" :: "" :: "" :: Nil,
			SplicePiece.Many[Id, Int](4 to 2 by -1) :: SplicePiece.Zero[Id]() :: SplicePiece.One[Id, Int](151) :: Nil), 4 :: 3 :: 2 :: 151 :: Nil)
	}
}
