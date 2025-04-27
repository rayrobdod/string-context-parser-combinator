package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.collection.mutable.Builder
import scala.collection.mutable.StringBuilder
import scala.quoted.*
import Repeated.SplicePiece

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[typeclass]
object RepeatedImpl {
	private[typeclass]
	final class ConcatenateString(using Quotes) extends Repeated[Expr[String], Expr[String]] {
		type Acc = List[Expr[String]]

		def init():Acc = Nil
		def append(acc:Acc, elem:Expr[String]):Acc = elem :: acc
		def result(acc:Acc):Expr[String] = {
			acc match {
				case List() => Expr[String]("")
				case List(elem) => elem
				case _ =>
					'{
						${
							acc.foldRight
								('{new scala.collection.mutable.StringBuilder})
								({(part, builder) => '{$builder.append($part)}})
						}
							.result
					}
			}
		}
	}

	private[typeclass]
	final class FromSplicesUsingBuilder[A, Z](
			newAccumulator: Expr[Builder[A, Z]],
			ifZero: Option[() => Expr[Z]],
			ifOneScalar: PartialFunction[Expr[A], Expr[Z]],
			ifOneSplice: PartialFunction[Expr[Iterable[A]], Expr[Z]]
			)(using Quotes, Type[A], Type[Z], Type[Builder[A, Z]],
	) extends Repeated[SplicePiece[Expr, A], Expr[Z]] {
		type Acc = List[SplicePiece[Expr, A]]

		def init():Acc = Nil
		def append(acc:Acc, elem:SplicePiece[Expr, A]):Acc = {
			elem match {
				case _: SplicePiece.Zero[Expr] => acc
				case other => other :: acc
			}
		}

		def result(acc:Acc):Expr[Z] = {
			acc match {
				case Nil => ifZero.map(_.apply()).getOrElse('{$newAccumulator.result()})
				case List(SplicePiece.One(elem)) => ifOneScalar.applyOrElse(elem, e => '{$newAccumulator.addOne(${e}).result()})
				case List(SplicePiece.Many(iter)) => ifOneSplice.applyOrElse(iter, es => '{$newAccumulator.addAll(${es}).result()})
				case _ =>
					'{
						val accumulator: Builder[A, Z] = ${newAccumulator}
						${Expr.block(
							acc.reverse.map({
								case SplicePiece.Zero() => '{()}
								case SplicePiece.One(elem) => '{accumulator.addOne($elem)}
								case SplicePiece.Many(iter) => '{accumulator.addAll($iter)}
							}),
							'{accumulator.result()}
						)}
					}
			}
		}
	}

	private[typeclass]
	def quotedFromSplicesToExprList[A](using Quotes, Type[A]): Repeated[SplicePiece[Expr, A], Expr[List[A]]] =
		new FromSplicesUsingBuilder[A, List[A]](
			'{ List.newBuilder },
			Option(() => '{ List.empty }),
			{(a: Expr[A]) => '{List($a)}},
			{case '{ $xs: List[A] } => xs},
		)

	private[typeclass]
	def quotedToExprList[A](using Quotes, Type[A]):BiRepeated[Expr, Expr[A], Expr[List[A]]] = {
		BiRepeated.apply[Expr, Expr[A], Builder[Expr[A], List[Expr[A]]], Expr[List[A]]](
			() => List.newBuilder[Expr[A]],
			(acc, elem) => {acc += elem},
			(acc) => Expr.ofList(acc.result()),
			PartialExprFunction[Expr, Expr[List[A]], (Expr[A], Expr[List[A]])](
				it => '{${it}.nonEmpty},
				it => ('{${it}.head}, '{${it}.tail}),
			),
			it => '{${it}.isEmpty},
		)
	}
}
