package com.wavesplatform.lang.directives
import com.wavesplatform.lang.directives.values.DirectiveValue

trait DirectiveDictionary[V <: DirectiveValue] {
  val default:      V
  val all:          Iterable[V]
  lazy val textMap: Map[String, V] = all.map(v => (v.text, v)).toMap
  lazy val idMap:   Map[Int, V]    = all.map(v => (v.id, v)).toMap
}

object DirectiveDictionary {
  def apply[V <: DirectiveValue](implicit dic: DirectiveDictionary[V]): DirectiveDictionary[V] = dic
}