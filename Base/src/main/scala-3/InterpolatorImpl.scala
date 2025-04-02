package name.rayrobdod.stringContextParserCombinator

import scala.quoted.*

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[stringContextParserCombinator]
object InterpolatorImpl {
	def stringContextFromExpr(sc:Expr[StringContext])(using Quotes): Seq[Expr[String]] = {
		sc match {
			case '{ _root_.scala.StringContext(${Varargs(args)}: _*) } => args
			case _ => scala.quoted.quotes.reflect.report.errorAndAbort(s"Do not know how to process this tree", sc)
		}
	}
}
