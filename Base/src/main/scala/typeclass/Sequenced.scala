package name.rayrobdod.stringContextParserCombinator
package typeclass

import com.eed3si9n.ifdef.ifdef

@ifdef("scalaEpochVersion:2")
private class NoWarnAboutUnusedIfdef {}

/**
 * Describes how to combine two adjacent values into one value
 *
 *
 * Below is example of defining and using a custom Sequenced:
 *
 * ```scala
 * import java.time._
 * //{
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Sequenced
 * //}
 *
 * given Sequenced[Any, LocalDate, LocalTime, LocalDateTime] with {
 *   def aggregate(date:LocalDate, time:LocalTime)(implicit ctx:Any):LocalDateTime = date.atTime(time)
 * }
 *
 * val dateParser:Interpolator[LocalDate] = ofType[LocalDate]
 * val timeParser:Interpolator[LocalTime] = ofType[LocalTime]
 * val p: Interpolator[LocalDateTime] = dateParser andThen timeParser
 *
 * p.interpolate(StringContext("", "", ""), LocalDate.of(2001, 02, 03) :: LocalTime.of(04, 05, 06) :: Nil) // `2001-02-03T04:05:06`: LocalDateTime
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.andThen Interpolator.andThen]]
 * @tparam A the first input
 * @tparam B the second input
 * @tparam Z the result container
 */
@FunctionalInterface
trait Sequenced[-Ctx, -A, -B, +Z] {
	def aggregate(left:A, right:B)(implicit ctx:Ctx):Z
}

/**
 * Describes how to separate a value into two adjacent values
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.andThen Extractor.andThen]]
 * @tparam A the first result
 * @tparam B the second result
 * @tparam Z the input value
 */
@FunctionalInterface
trait ContraSequenced[-Ctx, +A, +B, -Z] {
	def separate(value:Z)(implicit ctx:Ctx):(A, B)
}

/**
 * Describes how to combine and separate two adjacent values
 *
 * Combining is defined by the methods inherited from `Sequenced`,
 * while separating is defined by the methods inherited from `ContraSequenced`.
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.andThen Parser.andThen]]
 * @tparam A the first value
 * @tparam B the second value
 * @tparam Z the result container
 */
trait BiSequenced[-Ctx, A, B, Z]
		extends Sequenced[Ctx, A, B, Z]
		with ContraSequenced[Ctx, A, B, Z]

/**
 * Predefined implicit implementations of Sequenced
 */
object Sequenced extends LowPrioSequenced {
	def apply[Ctx, A, B, Z](aggregateFn:(A, B, Ctx) => Z):Sequenced[Ctx, A, B, Z] = {
		final class Apply extends Sequenced[Ctx, A, B, Z] {
			def aggregate(left:A, right:B)(implicit ctx:Ctx):Z = aggregateFn(left, right, ctx)
		}
		new Apply()
	}

	@ifdef("scalaBinaryVersion:3")
	def apply[Ctx, A, B, Z](aggregateFn:(A, B) => Ctx ?=> Z):Sequenced[Ctx, A, B, Z] = {
		final class Apply extends Sequenced[Ctx, A, B, Z] {
			def aggregate(left:A, right:B)(implicit ctx:Ctx):Z = aggregateFn(left, right)
		}
		new Apply()
	}

	/**
	 * Returns a Unit when both inputs are a Unit
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	 * val u1:Interpolator[Unit] = ???
	 * val u2:Interpolator[Unit] = ???
	 *
	 * //}
	 * ((u1:Interpolator[Unit]) andThen (u2:Interpolator[Unit])):Interpolator[Unit]
	 * ```
	 */
	implicit def unitUnit:Sequenced[Any, Unit, Unit, Unit] = BiSequenced.unitUnit
	/**
	 * Returns the non-unit input value
	 */
	implicit def unitGeneric[B]:Sequenced[Any, Unit, B, B] = BiSequenced.unitGeneric
	/**
	 * Returns the non-unit input value
	 */
	implicit def genericUnit[A]:Sequenced[Any, A, Unit, A] = BiSequenced.genericUnit
}

private[typeclass] trait LowPrioSequenced {
	/**
	 * The fallback Sequenced;
	 * returns the two inputs placed in a Tuple2
	 *
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.Interpolator
	 * class A {}
	 * class B {}
	 * val p1:Interpolator[A] = ???
	 * val p2:Interpolator[B] = ???
	 *
	 * //}
	 * ((p1:Interpolator[A]) andThen (p2:Interpolator[B])):Interpolator[(A, B)]
	 * ```
	 */
	implicit def toPair[A, B]:Sequenced[Any, A, B, (A, B)] = BiSequenced.toPair
}

/**
 * Predefined implicit implementations of ContraSequenced
 */
object ContraSequenced extends LowPrioContraSequenced {
	implicit def unitUnit:ContraSequenced[Any, Unit, Unit, Unit] = BiSequenced.unitUnit
	implicit def unitGeneric[B]:ContraSequenced[Any, Unit, B, B] = BiSequenced.unitGeneric
	implicit def genericUnit[A]:ContraSequenced[Any, A, Unit, A] = BiSequenced.genericUnit
}

private[typeclass] trait LowPrioContraSequenced {
	implicit def toPair[A, B]:ContraSequenced[Any, A, B, (A, B)] = BiSequenced.toPair
}

/**
 * Predefined implicit implementations of BiSequenced
 * and methods to create new BiSequenceds
 */
object BiSequenced extends LowPrioBiSequenced {
	private[typeclass] def apply[Ctx, A, B, Z](aggregateFn:(A, B, Ctx) => Z, separateFn:(Z, Ctx) => (A, B)):BiSequenced[Ctx, A, B, Z] = {
		final class Apply extends BiSequenced[Ctx, A, B, Z] {
			def aggregate(left:A, right:B)(implicit ctx:Ctx):Z = aggregateFn(left, right, ctx)
			def separate(value:Z)(implicit ctx:Ctx):(A, B) = separateFn(value, ctx)
		}
		new Apply()
	}

	implicit def unitUnit:BiSequenced[Any, Unit, Unit, Unit] = apply((_:Unit, _:Unit, _:Any) => (), (_:Unit, _:Any) => ((), ()))
	implicit def unitGeneric[B]:BiSequenced[Any, Unit, B, B] = apply((_:Unit, b:B, _:Any) => b, (b:B, _:Any) => ((), b))
	implicit def genericUnit[A]:BiSequenced[Any, A, Unit, A] = apply((a:A, _:Unit, _:Any) => a, (a:A, _:Any) => (a, ()))
}

private[typeclass] trait LowPrioBiSequenced {
	implicit def toPair[A, B]:BiSequenced[Any, A, B, (A, B)] = BiSequenced.apply((a:A, b:B, _:Any) => (a, b), (pair:(A, B), _:Any) => pair)
}
