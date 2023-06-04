package com.rayrobdod.stringContextParserCombinator

import scala.annotation.nowarn

/**
 * Scala 2.12 and Scala 2.13 disagree about whether an unused `<:<` parameter begets an error.
 * Meaning they disagree about whether an unused `<:<` parameter can be `@nowarn`ed.
 * Which means I have to move the nowarn into version-specific code.
 */
private[stringContextParserCombinator]
object TypeConformanceCompat {
	def contraSubstituteCo[F[+_], From, To](@nowarn self: <:<[From, To], ff: F[From]): F[To] = {
		ff.asInstanceOf[F[To]]
	}
	def contraSubstituteContra[F[-_], From, To](@nowarn self: <:<[From, To], ff: F[To]): F[From] = {
		ff.asInstanceOf[F[From]]
	}
}
