package com.rayrobdod.stringContextParserCombinator

/**
 * Thrown by [[Id]]-using parse methods when the parser fails to parse the string context
 *
 * The Expr-using parse methods will fail at compile time instead, and thus don't throw
 */
final class ParseException(msg:String) extends java.lang.RuntimeException(msg)
