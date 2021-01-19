---
---

StringContextParserCombinators is a library for writing String Context extension implementations.

# Example

```scala
import scala.quoted.{Expr, Quotes}
import com.rayrobdod.stringContextParserCombinator.Parsers._
import com.rayrobdod.stringContextParserCombinator.macroimpl

def eval1(folding:Expr[Int], elem:(Char, Expr[Int]))(using Quotes):Expr[Int] = elem._1 match {
	case '+' => '{${folding} + ${elem._2}}
	case '-' => '{${folding} - ${elem._2}}
	case '*' => '{${folding} * ${elem._2}}
	case '/' => '{${folding} / ${elem._2}}
}
def eval(head:Expr[Int], tail:Seq[(Char, Expr[Int])])(using Quotes):Expr[Int] = tail.foldLeft(head)(eval1 _)

def numberLiteral(using Quotes):Parser[Expr[Int]] = CharIn('0' to '9').repeat(1).map({x => Expr[Int](x.toInt)})
def numberProvided(using Quotes):Parser[Expr[Int]] = OfType[Int]
def parens(using Quotes):Parser[Expr[Int]] = DelayedConstruction(() => IsString("(") andThenWithCut addSub andThen IsString(")"))
def factor(using Quotes):Parser[Expr[Int]] = numberLiteral orElse numberProvided orElse parens

def divMul(using Quotes):Parser[Expr[Int]] = (factor andThen (CharIn("*/") andThenWithCut factor).repeat()).map(eval _)
def addSub(using Quotes):Parser[Expr[Int]] = (divMul andThen (CharIn("+-") andThenWithCut divMul).repeat()).map(eval _)
def expr(using Quotes):Parser[Expr[Int]] = addSub andThen End()

def stringContext_math_impl(sc:Expr[StringContext], args:Expr[Seq[Int]])(using Quotes):Expr[Int] = macroimpl(expr)(sc, args)
extension (inline sc:StringContext) inline def math(inline args:Int*):Int = ${stringContext_math_impl('sc, 'args)}
```

```scala
scala> math"1+2*3+4"
val res2: Int = 11

scala> math"1+${' '.toInt}"
val res3: Int = 33

scala> math"1+A"
1 |math"1+A"
  |       ^
  |       Found "A" ; Expected CharIn("0123456789") | scala.Int
```

# Entry Points

As for API, leaf parsers are avaliable from [the Parsers object](api/com/rayrobdod/stringContextParserCombinator/Parsers$.html)
and the created extension method implementation should ends up calling [the macroimpl method](api/com/rayrobdod/stringContextParserCombinator.html#macroimpl)
