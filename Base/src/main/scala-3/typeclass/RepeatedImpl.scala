package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.annotation.nowarn
import scala.collection.mutable.Builder
import scala.collection.mutable.StringBuilder
import scala.quoted.*
import Repeated.SplicePiece

// scala 2 reads the `'{Some($value}` as an unclosed character literal
// and ifdef is insufficient to hide that construct from the scala 2 compiler

private[typeclass]
object RepeatedImpl {
	private[typeclass]
	final class ConcatenateString extends Repeated[Quotes, Expr[String], Expr[String]] {
		type Acc = List[Expr[String]]

		def init()(implicit ctx:Quotes):Acc = Nil
		def append(acc:Acc, elem:Expr[String])(implicit ctx:Quotes):Acc = elem :: acc
		def result(acc:Acc)(implicit ctx:Quotes):Expr[String] = {
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
			newAccumulator: Quotes => Expr[Builder[A, Z]],
			ifZero: Quotes => Option[Expr[Z]],
			ifOneScalar: PartialFunction[(Expr[A], Quotes), Expr[Z]],
			ifOneSplice: PartialFunction[(Expr[Iterable[A]], Quotes), Expr[Z]]
			)(using TypeCreator[A], TypeCreator[Z],
	) extends Repeated[Quotes, SplicePiece[Expr, A], Expr[Z]] {
		type Acc = List[SplicePiece[Expr, A]]

		def init()(implicit ctx:Quotes):Acc = Nil
		def append(acc:Acc, elem:SplicePiece[Expr, A])(implicit ctx:Quotes):Acc = {
			elem match {
				case _: SplicePiece.Zero[Expr] => acc
				case other => other :: acc
			}
		}

		def result(acc:Acc)(implicit ctx:Quotes):Expr[Z] = {
			@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
			@nowarn("id=E198") given Type[Z] = TypeCreator[Z].createType

			acc match {
				case Nil =>
					ifZero(ctx).getOrElse('{${newAccumulator(ctx)}.result()})
				case List(SplicePiece.One(elem)) =>
					ifOneScalar.applyOrElse((elem, ctx), (e, ctx2) => '{${newAccumulator(ctx2)}.addOne(${e}).result()})
				case List(SplicePiece.Many(iter)) =>
					ifOneSplice.applyOrElse((iter, ctx), (es, ctx2) => '{${newAccumulator(ctx2)}.addAll(${es}).result()})
				case _ =>
					'{
						val accumulator: Builder[A, Z] = ${newAccumulator(ctx)}
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
	def quotedFromSplicesToExprList[A](using TypeCreator[A]): Repeated[Quotes, SplicePiece[Expr, A], Expr[List[A]]] =
		given TypeCreator[List[A]] = new TypeCreator[List[A]] {
			def createType(using Quotes) =
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				Type.of[List[A]]
		}

		new FromSplicesUsingBuilder[A, List[A]](
			{(ctx: Quotes) =>
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
				'{ List.newBuilder }
			},
			{(ctx: Quotes) =>
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
				Option('{ List.empty })
			},
			{case (a, ctx) =>
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
				'{List($a)}
			},
			new PartialFunction[(Expr[Iterable[A]], Quotes), Expr[List[A]]] {
				override def isDefinedAt(valueCtx: (Expr[Iterable[A]], Quotes)): Boolean = {
					val (value, ctx) = valueCtx
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
					value match {
						case '{ $xs: List[A] } => true
						case _ => false
					}
				}
				override def apply(valueCtx: (Expr[Iterable[A]], Quotes)): Expr[List[A]] = {
					val (value, ctx) = valueCtx
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
					value match {
						case '{ $xs: List[A] } => xs
					}
				}
				override def applyOrElse[A1 <: (Expr[Iterable[A]], Quotes), B1 >: Expr[List[A]]](valueCtx: A1, default: A1 => B1): B1 = {
					val (value, ctx) = valueCtx
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					@nowarn("id=E198") given Type[List[A]] = TypeCreator[List[A]].createType
					value match {
						case '{ $xs: List[A] } => xs
						case _ => default(valueCtx)
					}
				}
			},
		)

	private[typeclass]
	def quotedToExprList[A](using TypeCreator[A]):BiRepeated[Quotes, Expr, Expr[A], Expr[List[A]]] = {
		BiRepeated.apply[Quotes, Expr, Expr[A], Builder[Expr[A], List[Expr[A]]], Expr[List[A]]](
			(_) => List.newBuilder[Expr[A]],
			(acc, elem, _) => {acc += elem},
			(acc, ctx) => {
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				Expr.ofList(acc.result())
			},
			PartialExprFunction[Quotes, Expr, Expr[List[A]], (Expr[A], Expr[List[A]])](
				(it, ctx) => {
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					'{${it}.nonEmpty}
				},
				(it, ctx) => {
					given Quotes = ctx
					@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
					('{${it}.head}, '{${it}.tail})
				},
			),
			(it, ctx) => {
				given Quotes = ctx
				@nowarn("id=E198") given Type[A] = TypeCreator[A].createType
				'{${it}.isEmpty}
			},
		)
	}
}
