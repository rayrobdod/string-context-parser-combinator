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
		sealed trait Acc
		object AccZero extends Acc
		final class AccOne(val elem: Expr[String]) extends Acc
		final class AccMany extends Acc {
			val builder: Builder[Expr[String], Expr[String]] = List.newBuilder.mapResult(parts =>
				'{
					${
						parts.foldLeft
							('{new scala.collection.mutable.StringBuilder})
							({(builder, part) => '{$builder.append($part)}})
					}
						.result
				}
			)
		}

		def init():Acc = AccZero
		def append(acc:Acc, elem:Expr[String]):Acc = acc match {
			case AccZero => new AccOne(elem)
			case accOne: AccOne => {
				val retval = new AccMany()
				retval.builder += accOne.elem
				retval.builder += elem
				retval
			}
			case accMany: AccMany => {
				accMany.builder += elem
				accMany
			}
		}
		def result(acc:Acc):Expr[String] = {
			acc match {
				case AccZero => Expr[String]("")
				case accOne: AccOne => accOne.elem
				case accMany: AccMany => accMany.builder.result()
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
		sealed trait Acc
		object AccZero extends Acc
		final class AccOneScalar(val elem: Expr[A]) extends Acc
		final class AccOneSplice(val iter: Expr[Iterable[A]]) extends Acc
		final class AccMany extends Acc {
			val builder: Builder[Expr[Builder[A, Z]] => Expr[_], Expr[Z]] = List.newBuilder.mapResult(parts =>
				'{
					val accumulator: Builder[A, Z] = ${newAccumulator}
					${Expr.block(
						parts.map(part => part('accumulator)),
						'{accumulator.result()}
					)}
				}
			)
		}

		def init():Acc = AccZero
		def append(acc:Acc, elem:SplicePiece[Expr, A]):Acc = acc match {
			case AccZero =>
				elem match {
					case _: SplicePiece.Zero[Expr] => AccZero
					case elemOne: SplicePiece.One[Expr, A] => new AccOneScalar(elemOne.elem)
					case elemMany: SplicePiece.Many[Expr, A] => new AccOneSplice(elemMany.iter)
				}
			case accOne: AccOneScalar => {
				elem match {
					case _: SplicePiece.Zero[Expr] => accOne
					case elemOne: SplicePiece.One[Expr, A] =>
						val retval = new AccMany()
						retval.builder += {(acc) => '{$acc.addOne(${accOne.elem})}}
						retval.builder += {(acc) => '{$acc.addOne(${elemOne.elem})}}
						retval
					case elemMany: SplicePiece.Many[Expr, A] =>
						val retval = new AccMany()
						retval.builder += {(acc) => '{$acc.addOne(${accOne.elem})}}
						retval.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
						retval
				}
			}
			case accOne: AccOneSplice => {
				elem match {
					case _: SplicePiece.Zero[Expr] => accOne
					case elemOne: SplicePiece.One[Expr, A] =>
						val retval = new AccMany()
						retval.builder += {(acc) => '{$acc.addAll(${accOne.iter})}}
						retval.builder += {(acc) => '{$acc.addOne(${elemOne.elem})}}
						retval
					case elemMany: SplicePiece.Many[Expr, A] =>
						val retval = new AccMany()
						retval.builder += {(acc) => '{$acc.addAll(${accOne.iter})}}
						retval.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
						retval
				}
			}
			case accMany: AccMany => {
				elem match {
					case _: SplicePiece.Zero[Expr] =>
						// do nothing
					case elemOne: SplicePiece.One[Expr, A] =>
						accMany.builder += {(acc) => '{$acc.addOne(${elemOne.elem})}}
					case elemMany: SplicePiece.Many[Expr, A] =>
						accMany.builder += {(acc) => '{$acc.addAll(${elemMany.iter})}}
				}
				accMany
			}
		}

		def result(acc:Acc):Expr[Z] = {
			acc match {
				case AccZero => ifZero.map(_.apply()).getOrElse('{$newAccumulator.result()})
				case accOne: AccOneScalar => ifOneScalar.applyOrElse(accOne.elem, e => '{$newAccumulator.addOne(${e}).result()})
				case accOne: AccOneSplice => ifOneSplice.applyOrElse(accOne.iter, es => '{$newAccumulator.addAll(${es}).result()})
				case accMany: AccMany => accMany.builder.result()
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
