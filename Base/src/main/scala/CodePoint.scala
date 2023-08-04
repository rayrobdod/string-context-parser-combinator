package com.rayrobdod.stringContextParserCombinator

import scala.language.implicitConversions
import scala.math.Ordering

/**
 * Represents a unicode codepoint
 */
final case class CodePoint(val intValue:Int) {
	override def toString:String = new String(Array[Int](intValue), 0, 1)

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
	val MaxValue = Character.MAX_CODE_POINT
	val MinValue = Character.MIN_CODE_POINT

	implicit def char2Codepoint(c:Char):CodePoint = new CodePoint(c)

	implicit def orderingCodepoint:Ordering[CodePoint] = Ordering.Int.on(_.intValue)
}
