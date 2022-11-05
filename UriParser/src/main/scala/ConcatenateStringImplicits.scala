package com.rayrobdod.stringContextParserCombinatorExample.uri

import com.rayrobdod.stringContextParserCombinator.CodePoint
import com.rayrobdod.stringContextParserCombinator.typeclass._

/**
 * Instances of SCPC implicits that concatenate strings or default-value strings
 */
object ConcatenateStringImplicits {
	implicit val AndThenCodepointString: Sequenced[CodePoint, String, String] = new Sequenced[CodePoint, String, String] {
		def aggregate(a:CodePoint, b:String):String = s"${a}${b}"
	}

	implicit val AndThenStringCodepoint: Sequenced[String, CodePoint, String] = new Sequenced[String, CodePoint, String] {
		def aggregate(a:String, b:CodePoint):String = s"${a}${b}"
	}

	implicit val AndThenStringString: Sequenced[String, String, String] = new Sequenced[String, String, String] {
		def aggregate(a:String, b:String):String = s"${a}${b}"
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
