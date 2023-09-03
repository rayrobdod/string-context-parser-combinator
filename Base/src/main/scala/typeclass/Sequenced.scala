package name.rayrobdod.stringContextParserCombinator
package typeclass

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
 * given Sequenced[LocalDate, LocalTime, LocalDateTime] with {
 *   def aggregate(date:LocalDate, time:LocalTime):LocalDateTime = date.atTime(time)
 * }
 *
 * val dateParser:Interpolator[LocalDate] = ofType(using classOf[LocalDate])
 * val timeParser:Interpolator[LocalTime] = ofType(using classOf[LocalTime])
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
trait Sequenced[-A, -B, +Z] {
	def aggregate(left:A, right:B):Z
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
trait ContraSequenced[+A, +B, -Z] {
	def separate(value:Z):(A, B)
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
trait BiSequenced[A, B, Z]
		extends Sequenced[A, B, Z]
		with ContraSequenced[A, B, Z]

/**
 * Predefined implicit implementations of Sequenced
 */
object Sequenced extends LowPrioSequenced {
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
	implicit def unitUnit:Sequenced[Unit, Unit, Unit] = BiSequenced.unitUnit
	/**
	 * Returns the non-unit input value
	 */
	implicit def unitGeneric[B]:Sequenced[Unit, B, B] = BiSequenced.unitGeneric
	/**
	 * Returns the non-unit input value
	 */
	implicit def genericUnit[A]:Sequenced[A, Unit, A] = BiSequenced.genericUnit
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
	implicit def toPair[A, B]:Sequenced[A, B, (A, B)] = BiSequenced.toPair
}

/**
 * Predefined implicit implementations of ContraSequenced
 */
object ContraSequenced extends LowPrioContraSequenced {
	implicit def unitUnit:ContraSequenced[Unit, Unit, Unit] = BiSequenced.unitUnit
	implicit def unitGeneric[B]:ContraSequenced[Unit, B, B] = BiSequenced.unitGeneric
	implicit def genericUnit[A]:ContraSequenced[A, Unit, A] = BiSequenced.genericUnit
}

private[typeclass] trait LowPrioContraSequenced {
	implicit def toPair[A, B]:ContraSequenced[A, B, (A, B)] = BiSequenced.toPair
}

/**
 * Predefined implicit implementations of BiSequenced
 * and methods to create new BiSequenceds
 */
object BiSequenced extends LowPrioBiSequenced {
	private[typeclass] def apply[A, B, Z](aggregateFn:(A, B) => Z, separateFn:Z => (A, B)):BiSequenced[A, B, Z] = {
		final class Apply extends BiSequenced[A, B, Z] {
			def aggregate(left:A, right:B):Z = aggregateFn(left, right)
			def separate(value:Z):(A, B) = separateFn(value)
		}
		new Apply()
	}

	implicit def unitUnit:BiSequenced[Unit, Unit, Unit] = apply((_:Unit, _:Unit) => (), (_:Unit) => ((), ()))
	implicit def unitGeneric[B]:BiSequenced[Unit, B, B] = apply((_:Unit, b:B) => b, (b:B) => ((), b))
	implicit def genericUnit[A]:BiSequenced[A, Unit, A] = apply((a:A, _:Unit) => a, (a:A) => (a, ()))
}

private[typeclass] trait LowPrioBiSequenced {
	implicit def toPair[A, B]:BiSequenced[A, B, (A, B)] = BiSequenced.apply((a:A, b:B) => (a, b), Predef.identity _)
}
