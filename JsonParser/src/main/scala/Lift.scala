package com.rayrobdod.stringContextParserCombinatorExample.json

import org.json4s.JsonAST._

/**
 * A conversion from `A` to `Z` that, if found via implicit search, is treated as automatic by the
 * json interpolation.
 *
 * Is a parameterized Lift class instead of one lift per `Z` mostly as an optimization:
 * * so that all Lifts can all share the 'JValue' converter
 * * and to reduce class count
 */
trait Lift[-A, +Z]{
	def apply(value:A):Z
}

/**
 * Predefined given `Lift` values
 */
object Lift {
	def apply[A, Z](fn:Function1[A, Z]):Lift[A, Z] = new Lift[A, Z] {def apply(value:A) = fn(value)}

	/** allows any jvalue subclass to lift to itself */
	implicit def jvalue[A <: JValue]:Lift[A, A] =
		Lift(scala.Predef.identity)

	type Boolean[A] = Lift[A, JBool]
	implicit val bool:Lift[scala.Boolean, JBool] =
		Lift( JBool.apply _ )

	type Number[A] = Lift[A, JValue with JNumber]
	implicit val int:Lift[scala.Int, JValue with JNumber] =
		Lift( x => JLong.apply(x) )
	implicit val long:Lift[scala.Long, JValue with JNumber] =
		Lift( JLong.apply _ )
	implicit val bigint:Lift[scala.math.BigInt, JValue with JNumber] =
		Lift( JInt.apply _ )
	implicit val bigdecimal:Lift[scala.math.BigDecimal, JValue with JNumber] =
		Lift( JDecimal.apply _ )

	type String[A] = Lift[A, JString]
	implicit val string:Lift[java.lang.String, JString] =
		Lift( JString.apply _ )

	type Array[A] = Lift[A, JArray]
	implicit def seq[A](implicit child:Lift[A, JValue]):Lift[scala.collection.immutable.Seq[A], JArray] =
		Lift[scala.collection.immutable.Seq[A], JArray](
			xs => JArray(xs.map(child.apply _).toList)
		)

	type KeyValue[A] = Lift[A, (java.lang.String, JValue)]
	implicit def keyValue[K, V](implicit liftKey:Lift[K, JString], liftValue:Lift[V, JValue]):Lift[Tuple2[K, V], Tuple2[java.lang.String, JValue]] =
		Lift[Tuple2[K, V], Tuple2[java.lang.String, JValue]](
			kv => (liftKey(kv._1).values, liftValue(kv._2))
		)

	type Object[A] = Lift[A, JObject]
	implicit def map[A](implicit child:Lift[A, JValue]):Lift[scala.collection.immutable.Map[java.lang.String, A], JObject] =
		Lift[scala.collection.immutable.Map[java.lang.String, A], JObject](
			xs => JObject(xs.mapValues(child.apply _).toList)
		)
}
