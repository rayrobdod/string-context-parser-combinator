package name.rayrobdod.stringContextParserCombinatorExample.uri

import name.rayrobdod.stringContextParserCombinator.CodePoint
import name.rayrobdod.stringContextParserCombinator.typeclass._

/**
 * Instances of SCPC implicits that concatenate strings or default-value strings
 */
object ConcatenateStringImplicits {
	implicit val AndThenCodepointString: Sequenced[Any, CodePoint, String, String] = Sequenced({
		(a:CodePoint, b:String, _:Any) => s"${a}${b}"
	})

	implicit val AndThenStringCodepoint: Sequenced[Any, String, CodePoint, String] = Sequenced({
		(a:String, b:CodePoint, _:Any) => s"${a}${b}"
	})

	implicit val AndThenStringString: Sequenced[Any, String, String, String] = Sequenced({
		(a:String, b:String, _:Any) => s"${a}${b}"
	})

	implicit val EmptyStringOptionallyTypes: Optionally[Any, String, String] = Optionally.whereDefault[Any, String](_ => "")

	implicit val CodePointOptionallyTypes: Optionally[Any, CodePoint, String] = Optionally[Any, CodePoint, String](_ => "", (x, _) => x.toString)

	implicit val StringRepeatTypes: Repeated[Any, String, String] = new Repeated[Any, String, String] {
		type Acc = StringBuilder
		def init()(implicit ctx:Any):Acc = new StringBuilder
		def append(acc:Acc, elem:String)(implicit ctx:Any):Acc = {acc ++= elem}
		def result(acc:Acc)(implicit ctx:Any):String = acc.toString
	}
}
