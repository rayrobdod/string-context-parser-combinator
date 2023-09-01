package name.rayrobdod.stringContextParserCombinator

/**
 * The input to a [[Parser]]
 */
private[stringContextParserCombinator]
final class Input[+Expr, Pos : Position : Ordering](
	private val parts:List[(String, Pos)],
	private val args:List[(Expr, Pos)]
) {
	private[stringContextParserCombinator] def position:Pos = {
		if (this.parts(0)._1.length != 0) {
			this.parts(0)._2
		} else if (this.args.nonEmpty) {
			this.args(0)._2
		} else {
			this.parts(0)._2
		}
	}

	/**
	 *
	 *
	 * @param partsFn called if the next value in the input is a `part`. A `Some` indicates a successful parse.
	 *		The `Int` in the return value is the number of characters in the string that the parser consumes.
	 * @param argsFn called if the next value in the input is an `arg`. A `Some` indicates a successful parse.
	 * @param expecting A textual description of what input the parser will parse successfully
	 */
	private[stringContextParserCombinator] def consume[A](
		partsFn:String => Option[(A, Int)],
		argsFn:Expr => Option[A],
		expecting: ExpectingDescription
	):Result[Expr, Pos, A] = {
		val expectingPosition = Expecting(expecting, this.position)
		val expectingPositionSet = ExpectingSet(expectingPosition)
		def failure = Failure(expectingPositionSet)
		if (parts.head._1.isEmpty) {
			if (args.nonEmpty) {
				def success(x:A) = Success(x, new Input(parts.tail, args.tail), expectingPositionSet)
				argsFn(args.head._1).fold[Result[Expr, Pos, A]](failure)(success _)
			} else {
				failure
			}
		} else {
			val (headStr, headPos) = parts.head
			def success(x:(A, Int)) = Success(x._1, new Input((headStr.substring(x._2), headPos + x._2) :: parts.tail, args), expectingPositionSet)
			partsFn(headStr).fold[Result[Expr, Pos, A]](failure)(success _)
		}
	}

	/**
	 * Returns a {@link Success} if this Input is empty; otherwise a
	 * {@link Failure}
	 */
	private[stringContextParserCombinator] def isEmpty:Boolean = parts.head._1.isEmpty && args.isEmpty

	/**
	 * Feeds the first `part` of the input to the given interpolator; returns the parse result
	 *
	 * Since the arguments are temporarily ignored, allows a parser with a different `Expr` type to attempt to parse this input,
	 * such as during the [[ExtractorAtom]] parsing
	 */
	private[stringContextParserCombinator]
	def justCurrentPartConsume[Expr2, A](parser:internal.Interpolator[Expr2, A]):Result[Expr, Pos, A] = {
		parser.interpolate(new Input[Nothing, Pos](this.parts.head :: Nil, Nil)) match {
			case failure:Failure[Pos] => failure
			case success:Success[Nothing, Pos, A] => success.map({
				case Success1(value, remaining, expecting) => {
					Success1(
						value,
						new Input(remaining.parts.head :: this.parts.tail, this.args),
						expecting
					)
				}
			})
		}
	}

	override def toString:String = s"Input(${parts}, ${args})"
	override def hashCode:Int = java.util.Objects.hash(parts, args)
	override def equals(rhs:Any):Boolean = rhs match {
		case other:Input[_, _] => this.parts == other.parts && this.args == other.args
		case _ => false
	}
}
