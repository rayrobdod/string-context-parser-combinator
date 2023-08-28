package com.rayrobdod.stringContextParserCombinatorExample.json

import org.json4s.{JValue, JBool, JNumber, JLong, JInt, JDecimal, JString, JArray, JObject}

/**
 * A conversion from `A` to `Z` that, if found via implicit search, is treated as automatic by the
 * json interpolation.
 */
trait Lift[-A, +Z]{
	def apply(value:A):Z
}

/**
 * Predefined given `Lift` values, and type aliases for Lift that specify the `Z` type
 */
object Lift {
	/** allows any jvalue subclass to lift to itself */
	implicit def jvalue[A <: JValue]:Lift[A, A] =
		x => x

	type Boolean[A] = Lift[A, JBool]
	implicit val bool:Lift[scala.Boolean, JBool] =
		JBool.apply _

	type Number[A] = Lift[A, JValue with JNumber]
	implicit val int:Lift[scala.Int, JValue with JNumber] =
		x => JLong.apply(x)
	implicit val long:Lift[scala.Long, JValue with JNumber] =
		JLong.apply _
	implicit val bigint:Lift[scala.math.BigInt, JValue with JNumber] =
		JInt.apply _
	implicit val bigdecimal:Lift[scala.math.BigDecimal, JValue with JNumber] =
		JDecimal.apply _

	type String[A] = Lift[A, JString]
	implicit val string:Lift[java.lang.String, JString] =
		JString.apply _

	type Array[A] = Lift[A, JArray]
	implicit def seq[A](implicit child:Lift[A, JValue]):Lift[scala.collection.immutable.Seq[A], JArray] =
		xs => JArray(xs.map(child.apply _).toList)

	type KeyValue[A] = Lift[A, (java.lang.String, JValue)]
	implicit def keyValue[K, V](implicit liftKey:Lift[K, JString], liftValue:Lift[V, JValue]):Lift[Tuple2[K, V], Tuple2[java.lang.String, JValue]] =
		kv => (liftKey(kv._1).values, liftValue(kv._2))

	type Object[A] = Lift[A, JObject]
	implicit def map[A](implicit child:Lift[A, JValue]):Lift[scala.collection.immutable.Map[java.lang.String, A], JObject] =
		// 2.12 doesn't have `xs.view.mapValues`; 2.13 deprecates `xs.mapValues` without the view
		xs => JObject(xs.view.map({x => ((x._1, child(x._2)))}).toList)

	type Value[A] = Lift[A, JValue]
}
