package name.rayrobdod.stringContextParserCombinator

import scala.quoted._

/** Represents a position in a source file. Indicates where to point to in compile error messages */
private[stringContextParserCombinator]
trait Position[Pos] {
	extension (pos:Pos) def +(offset:Int):Pos = this.offset(pos, offset)
	def offset(pos:Pos, offset:Int):Pos
}

private[stringContextParserCombinator]
object PositionGivens {
	// Probably can assume that any positions compared will have the same sourceFile
	given given_QuotedPosition_Ordering(using q:Quotes):Ordering[q.reflect.Position] = Ordering.by(_.start)

	given given_QuotedPosition_Position(using q:Quotes):Position[q.reflect.Position] = {
		(pos:q.reflect.Position, offset:Int) => {
			q.reflect.Position(pos.sourceFile, pos.start + offset, pos.end)
		}
	}

	val given_IdPosition_Position:Position[Int] = {(pos:Int, offset:Int) => pos + offset}
}
