package name.rayrobdod.stringContextParserCombinator
package typeclass

/**
 * Describes how to represent a result that may be one of two results
 *
 * Below is example of defining and using a custom Eithered.
 *
 * ```scala
 * import java.io.File
 * import java.net.URI
 * import java.util.UUID
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
 *
 * given Eithered[File, UUID, URI] with {
 *   def left(f: File): URI = f.toURI
 *   def right(id: UUID): URI = new URI("urn", "uuid:" + id.toString, null)
 * }
 *
 * val uuidParser:Interpolator[UUID] = ofType[UUID]
 * val fileParser:Interpolator[File] = ofType[File]
 * val p:Interpolator[URI] = (fileParser:Interpolator[File]) orElse (uuidParser:Interpolator[UUID]) // using Eithered[File, UUID, URI]
 *
 * p.interpolate(StringContext("", ""), new File("/tmp") :: Nil) // `file:///tmp`: URI
 * p.interpolate(StringContext("", ""), UUID.randomUUID() :: Nil) // `urn:uuid:429bf7eb-650e-4f8c-be3f-1420913a6bd7`: URI
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.orElse Interpolator.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait Eithered[-A, -B, +Z] {
	def left(elem:A):Z
	def right(elem:B):Z
}

/**
 * Describes how to disambiguate the union of two types
 *
 * The parser determines whether the left or right branch is taken.
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.orElse Extractor.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait ContraEithered[+Expr[_], +A, +B, -Z] {
	def contraLeft:PartialExprFunction[Expr, Z, A]
	def contraRight:PartialExprFunction[Expr, Z, B]
}

/**
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.orElse Parser.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait BiEithered[Expr[_], A, B, Z]
		extends Eithered[A, B, Z]
		with ContraEithered[Expr, A, B, Z]

/**
 * Predefined implicit implementations of Eithered
 * and methods to create new Eithereds
 */
object Eithered extends VersionSpecificEithered {
	/**
	 * Constructs an `Eithered` from a set of functions corresponding to each of Eithered's methods
	 */
	def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	/**
	 * An Eithered that wraps the value in a `scala.Either`

	 * @example
	 * In the following interpolator, even digits are placed in a Left while odd digits are placed in a Right
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
	 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
	 *
	 * //}
	 * val evenOdd:Interpolator[Either[Char, Char]] = charIn("02468").orElse(charIn("13579"))(using Eithered.discriminatedUnion)
	 *
	 * evenOdd.interpolate(StringContext("4"), Nil) // Left(4): Either[Char, Char]
	 * evenOdd.interpolate(StringContext("7"), Nil) // Right(7): Either[Char, Char]
	 * ```
	 */
	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

/**
 * Predefined implicit implementations of ContraEithered
 * and methods to create new ContraEithereds
 */
object ContraEithered extends VersionSpecificContraEithered {
	/**
	 * Constructs an `ContraEithered` from a set of functions corresponding to each of ContraEithered's methods
	 */
	def apply[Expr[_], A, B, Z](
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):ContraEithered[Expr, A, B, Z] = {
		final class Apply extends ContraEithered[Expr, A, B, Z] {
			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}
}

/**
 * Predefined implicit implementations of BiEithered
 * and methods to create new BiEithereds
 */
object BiEithered extends VersionSpecificBiEithered {
	/**
	 * Constructs an `BiEithered` from a set of functions corresponding to each of BiEithered's methods
	 */
	def apply[Expr[_], A, B, Z](
		leftFn:A => Z,
		rightFn:B => Z,
		contraLeftFn:PartialExprFunction[Expr, Z, A],
		contraRightFn:PartialExprFunction[Expr, Z, B]
	):BiEithered[Expr, A, B, Z] = {
		final class Apply extends BiEithered[Expr, A, B, Z] {
			override def left(elem:A):Z = leftFn(elem)
			override def right(elem:B):Z = rightFn(elem)

			override def contraLeft:PartialExprFunction[Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

}
