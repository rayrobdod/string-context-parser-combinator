package name.rayrobdod.stringContextParserCombinator

import scala.math.Ordering

/**
 * Represents a unicode codepoint
 */
final class CodePoint private (val intValue:Int) {
	if (! Character.isValidCodePoint(intValue)) throw new IllegalArgumentException(s"$intValue")

	override def toString:String = new String(Array[Int](intValue), 0, 1)
	override def hashCode:Int = intValue
	override def equals(other:Any):Boolean = other match {
		case other2:CodePoint => this.intValue == other2.intValue
		case _ => false
	}

	def charCount:Int = Character.charCount(intValue)
	def name:String = Character.getName(intValue)

	def isAlphabetic:Boolean = Character.isAlphabetic(intValue)
	def isBmp:Boolean = Character.isBmpCodePoint(intValue)
	def isDefined:Boolean = Character.isDefined(intValue)
	def isDigit:Boolean = Character.isDigit(intValue)
	def isIdentifierIgnorable:Boolean = Character.isDigit(intValue)
	def isIdeographic:Boolean = Character.isIdeographic(intValue)
	def isISOControl:Boolean = Character.isISOControl(intValue)
	def isJavaIdentifierPart:Boolean = Character.isJavaIdentifierPart(intValue)
	def isJavaIdentifierStart:Boolean = Character.isJavaIdentifierStart(intValue)
	def isLetter:Boolean = Character.isLetter(intValue)
	def isLetterOrDigit:Boolean = Character.isLetterOrDigit(intValue)
	def isLowerCase:Boolean = Character.isLowerCase(intValue)
	def isMirrored:Boolean = Character.isMirrored(intValue)
	def isSpaceChar:Boolean = Character.isSpaceChar(intValue)
	def isSupplementary:Boolean = Character.isSupplementaryCodePoint(intValue)
	def isTitleCase:Boolean = Character.isTitleCase(intValue)
	def isUnicodeIdentifierPart:Boolean = Character.isUnicodeIdentifierPart(intValue)
	def isUnicodeIdentifierStart:Boolean = Character.isUnicodeIdentifierStart(intValue)
	def isUpperCase:Boolean = Character.isUpperCase(intValue)
	def isWhitespace:Boolean = Character.isWhitespace(intValue)

	def toLowerCase:CodePoint = new CodePoint(Character.toLowerCase(intValue))
	def toTitleCase:CodePoint = new CodePoint(Character.toTitleCase(intValue))
	def toUpperCase:CodePoint = new CodePoint(Character.toUpperCase(intValue))
}

object CodePoint {
	val MaxValue:CodePoint = new CodePoint(Character.MAX_CODE_POINT)
	val MinValue:CodePoint = new CodePoint(Character.MIN_CODE_POINT)

	def apply(cp:Char):CodePoint = new CodePoint(cp.toInt)
	def apply(cp:Int):Option[CodePoint] = if (Character.isValidCodePoint(cp)) Option(new CodePoint(cp)) else None

	def unsafe_apply(cp:Int):CodePoint = new CodePoint(cp)

	implicit def char2Codepoint(c:Char):CodePoint = new CodePoint(c.toInt)

	implicit def orderingCodepoint:Ordering[CodePoint] = Ordering.Int.on(_.intValue)
}
