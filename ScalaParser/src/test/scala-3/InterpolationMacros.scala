package name.rayrobdod.stringContextParserCombinatorExample.quasiquotesTests

import scala.annotation.unused
import scala.quoted.*
import name.rayrobdod.stringContextParserCombinatorExample.quasiquotes.*

object InterpolationMacros:
	extension [A] (a: Expr[A]) @unused private def showInfix(using Quotes) = {quotes.reflect.report.info(a.show); a}

	def splicedValueImpl[A](v: Expr[A])(using Quotes, Type[A]): Expr[A] = q"${v}".asExprOf[A]
	def liftedStringImpl(v: String)(using Quotes): Expr[String] = q"${v}".asExprOf[String]
	def nullImpl(using Quotes): Expr[Null] = q"null".asExprOf[Null]
	def trueImpl(using Quotes): Expr[Boolean] = q"true".asExprOf[Boolean]
	def falseImpl(using Quotes): Expr[Boolean] = q"false".asExprOf[Boolean]
	def charAImpl(using Quotes): Expr[Char] = q"'a'".asExprOf[Char]
	def charNewlineImpl(using Quotes): Expr[Char] = q"'\n'".asExprOf[Char]
	def charUnicodeImpl(using Quotes): Expr[Char] = q"'\u1234'".asExprOf[Char]
	def zeroImpl(using Quotes): Expr[Int] = q"0".asExprOf[Int]
	def oneImpl(using Quotes): Expr[Int] = q"1".asExprOf[Int]
	def negOneImpl(using Quotes): Expr[Int] = q"-1".asExprOf[Int]
	def binIntImpl(using Quotes): Expr[Int] = q"0b1010_1100".asExprOf[Int]
	def decIntImpl(using Quotes): Expr[Int] = q"0_1__2_3".asExprOf[Int]
	def hexIntImpl(using Quotes): Expr[Int] = q"0x12_34_56".asExprOf[Int]
	def hexLongImpl(using Quotes): Expr[Long] = q"0x1234_5678_9ABC_DEF0L".asExprOf[Long]
	def emptyStringImpl(using Quotes): Expr[String] = q""" "" """.asExprOf[String]
	def asciiStringImpl(using Quotes): Expr[String] = q""" "Hello World" """.asExprOf[String]
	def escapesStringImpl(using Quotes): Expr[String] = q""" "\u0041\n\u0042" """.asExprOf[String]
	def notTrueImpl(using Quotes): Expr[Boolean] = q"! true".asExprOf[Boolean]
	def intToFloat(using Quotes): Expr[Float] = q"25.toFloat".asExprOf[Float]
	def rangeTripleTail(v: Expr[Range])(using Quotes): Expr[Range] = q"${v}.tail.tail.tail".asExprOf[Range]
