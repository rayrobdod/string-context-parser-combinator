package name.rayrobdod.stringContextParserCombinator

/**
 * Scala 2.12 and Scala 2.13 disagree about whether an unused `<:<` parameter begets an error.
 * Meaning they disagree about whether an unused `<:<` parameter can be `@nowarn`ed.
 * Which means I have to move the nowarn into version-specific code.
 */
private[stringContextParserCombinator]
object TypeConformanceCompat {
	def contraSubstituteCo[F[+_], From, To](self: <:<[From, To], ff: F[From]): F[To] = {
		self.substituteCo[F](ff)
	}
	def contraSubstituteContra[F[-_], From, To](self: <:<[From, To], ff: F[To]): F[From] = {
		self.substituteContra[F](ff)
	}

	def equivFlip[From, To](self: =:=[From, To]): =:=[To, From] = {
		self.flip
	}
}
