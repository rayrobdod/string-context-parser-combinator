---
---

This is a scala library for writing [custom string interpolation](https://docs.scala-lang.org/scala3/book/string-interpolation.html#advanced-usage) implementations via parser combinators.

# Example

```scala
import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._

extension (sc: StringContext)
  def math(args: Int*): Int =
    def eval1(folding:Int, elem:(Char, Int)):Int = elem._1 match {
      case '+' => folding + elem._2
      case '-' => folding - elem._2
      case '*' => folding * elem._2
      case '/' => folding / elem._2
    }
    def eval(head:Int, tail:Seq[(Char, Int)]):Int = tail.foldLeft(head)(eval1 _)
    //
    def numberLiteral:Interpolator[Int] = charIn('0' to '9').repeat(1).map({x => x.toInt})
    def numberProvided:Interpolator[Int] = ofType[Integer].map({x => x:Int})
    def parens:Interpolator[Int] = `lazy`(() => isString("(") andThen addSub andThen isString(")"))
    def factor:Interpolator[Int] = numberLiteral orElse numberProvided orElse parens
    def divMul:Interpolator[Int] = (factor andThen (charIn("*/") andThen factor).repeat()).map(eval _)
    def addSub:Interpolator[Int] = (divMul andThen (charIn("+-") andThen divMul).repeat()).map(eval _)
    def expr:Interpolator[Int] = addSub andThen end
    //
    expr.interpolate(sc, args)


math"1+2*3+4" // 11: Int
math"(1+2)*(3+4)" // 21: Int
math"1+${' '.toInt}" // 33: Int
math"1+A" // Expected "(" or CharIn("0123456789") or OfType(java.lang.Integer)
```

More complicated examples are available as subprojects of the repository
