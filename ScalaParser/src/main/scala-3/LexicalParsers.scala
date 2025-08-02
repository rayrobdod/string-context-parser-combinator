package name.rayrobdod.stringContextParserCombinatorExample.quasiquotes

import scala.math.Ordering.Implicits.infixOrderingOps
import name.rayrobdod.stringContextParserCombinator.CodePoint
import name.rayrobdod.stringContextParserCombinator.Interpolator.quotedInterpolators._
import name.rayrobdod.stringContextParserCombinator.typeclass.*

object LexicalParsers:
	private given Sequenced[Any, CodePoint, String, String] = new:
		def aggregate(_1: CodePoint, _2: String)(implicit ctx: Any) = s"$_1$_2"
	private given Sequenced[Any, String, String, String] = new:
		def aggregate(_1: String, _2: String)(implicit ctx: Any) = s"$_1$_2"
	private given Optionally[Any, String, String] = Optionally.whereDefault(_ => "")
	private given Eithered[Any, CodePoint, Char, CodePoint] = Eithered((a, _) => a, (b, _) => CodePoint(b))

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

	private[quasiquotes] val upper: Interpolator[CodePoint] = codePointWhere: c =>
		CodePoint('$') == c || (CodePoint('A') <= c && c <= CodePoint('Z')) || {
			val category = c.category
			category == Character.UPPERCASE_LETTER ||
				category == Character.TITLECASE_LETTER ||
				category == Character.LETTER_NUMBER
		}
	private[quasiquotes] val lower: Interpolator[CodePoint] = codePointWhere: c =>
		CodePoint('_') == c || (CodePoint('a') <= c && c <= CodePoint('z')) || {
			val category = c.category
			category == Character.LOWERCASE_LETTER
		}
	private[quasiquotes] val letter: Interpolator[CodePoint] = upper <|> lower

	private[quasiquotes] val opchar: Interpolator[CodePoint] = codePointWhere: c =>
		"!#%&*+-/:<=>?@\\^|~".contains(c) || {
			val category = c.category
			category == Character.MATH_SYMBOL ||
				category == Character.OTHER_SYMBOL
		}
	private[quasiquotes] val op: Interpolator[String] = opchar.repeat(1)

	private[quasiquotes] val idrest: Interpolator[String] = (letter <|> decimalDigit).repeat() <~> (codePointIn("_") <~> op).optionally()
	private[quasiquotes] val alphaid: Interpolator[String] = (letter <~> idrest)
	private[quasiquotes] val plainid: Interpolator[String] = op <|> alphaid
	private[quasiquotes] val id: Interpolator[String] = plainid
end LexicalParsers
