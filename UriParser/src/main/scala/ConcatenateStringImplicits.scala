package name.rayrobdod.stringContextParserCombinatorExample.uri

import name.rayrobdod.stringContextParserCombinator.CodePoint
import name.rayrobdod.stringContextParserCombinator.typeclass._

/**
 * Instances of SCPC implicits that concatenate strings or default-value strings
 */
object ConcatenateStringImplicits {
	implicit val AndThenCodepointString: Sequenced[CodePoint, String, String] = {
		(a:CodePoint, b:String) => s"${a}${b}"
	}

	implicit val AndThenStringCodepoint: Sequenced[String, CodePoint, String] = {
		(a:String, b:CodePoint) => s"${a}${b}"
	}

	implicit val AndThenStringString: Sequenced[String, String, String] = {
		(a:String, b:String) => s"${a}${b}"
	}

	implicit val EmptyStringOptionallyTypes: Optionally[String, String] = Optionally.whereDefault[String]("")

	implicit val CodePointOptionallyTypes: Optionally[CodePoint, String] = Optionally[CodePoint, String]("", _.toString)

	implicit val StringRepeatTypes: Repeated[String, String] = new Repeated[String, String] {
		type Acc = StringBuilder
		def init():Acc = new StringBuilder
		def append(acc:Acc, elem:String):Unit = {acc ++= elem}
		def result(acc:Acc):String = acc.toString
	}
}
