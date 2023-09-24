package name.rayrobdod.stringContextParserCombinator

import scala.reflect.macros.blackbox.Context

/** Represents a position in a source. Indicates where to point to in compile error messages */
private[stringContextParserCombinator]
trait Position[Pos] {
	def offset(pos:Pos, offset:Int):Pos
}

private[stringContextParserCombinator]
object PositionGivens {
	def given_ExprPosition_Position(c:Context):Position[c.universe.Position] = {
		(pos:c.universe.Position, offset:Int) => {
			pos.withPoint(pos.start + offset)
		}
	}

	// Probably can assume that any positions compared will have the same sourceFile
	def given_ExprPosition_Ordering(c:Context):Ordering[c.universe.Position] = Ordering.by(_.start)

	val given_IdPosition_Position:Position[Int] = {(pos:Int, offset:Int) => pos + offset}
}
