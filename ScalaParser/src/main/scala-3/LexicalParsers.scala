package name.rayrobdod.stringContextParserCombinatorExample.quasiquotes

import name.rayrobdod.stringContextParserCombinator.Interpolator.quotedInterpolators._

object LexicalParsers:
	private[quasiquotes] val whitespaces: Interpolator[Unit] =
		charIn("\n\r\t ")
			.void
			.repeat()
			.hide

	private[quasiquotes] val binaryDigit: Interpolator[Char] =
		charWhere(c => ('0' <= c && c <= '1'))
	private[quasiquotes] val decimalDigit: Interpolator[Char] =
		charWhere(c => ('0' <= c && c <= '9'))
	private[quasiquotes] val hexDigit: Interpolator[Char] =
		charWhere(c => ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'))
