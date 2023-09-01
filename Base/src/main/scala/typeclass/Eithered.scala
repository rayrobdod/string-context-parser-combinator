package name.rayrobdod.stringContextParserCombinator
package typeclass

/**
 * Describes how to represent the union of two types
 *
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
 */
trait ContraEithered[+Expr[_], +A, +B, -Z] {
	def contraLeft:PartialExprFunction[Expr, Z, A]
	def contraRight:PartialExprFunction[Expr, Z, B]
}

trait BiEithered[Expr[_], A, B, Z]
		extends Eithered[A, B, Z]
		with ContraEithered[Expr, A, B, Z]

/** Predefined implicit implementations of Eithered */
object Eithered extends VersionSpecificEithered {
	def apply[A, B, Z](leftFn:A => Z, rightFn:B => Z):Eithered[A, B, Z] = {
		final class Apply extends Eithered[A, B, Z] {
			def left(elem:A):Z = leftFn(elem)
			def right(elem:B):Z = rightFn(elem)
		}
		new Apply()
	}

	def discriminatedUnion[A, B]:Eithered[A, B, Either[A, B]] = Eithered(Left.apply _, Right.apply _)
}

/** Predefined implicit implementations of ContraEithered */
object ContraEithered extends VersionSpecificContraEithered {
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

/** Predefined implicit implementations of BiEithered */
object BiEithered extends VersionSpecificBiEithered {
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
