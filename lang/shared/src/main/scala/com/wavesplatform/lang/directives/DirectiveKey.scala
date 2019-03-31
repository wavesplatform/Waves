package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values._

trait DirectiveKey {
  type Value <: DirectiveValue
  val text: String
  val valueDic: DirectiveDictionary[Value]

  override def toString: String = text
}

object DirectiveKey {
  def apply[V <: DirectiveValue](value: String)(implicit dic: DirectiveDictionary[V]) = new DirectiveKey {
    override val text: String = value
    override type Value = V
    override implicit val valueDic: DirectiveDictionary[V] = dic
  }

  val STDLIB_VERSION = DirectiveKey[StdLibVersion]("STDLIB_VERSION")
  val CONTENT_TYPE   = DirectiveKey[ContentType]("CONTENT_TYPE")
  val SCRIPT_TYPE    = DirectiveKey[ScriptType]("CONTENT_TYPE")

  val all: Set[DirectiveKey] =
    Set(
      STDLIB_VERSION,
      CONTENT_TYPE,
      SCRIPT_TYPE
    )

  val textMap: Map[String, DirectiveKey] = all.map(d => (d.text, d)).toMap
}
