package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values.*

sealed trait DirectiveKey {
  type Value <: DirectiveValue
  def text: String
}

trait PredefinedDirectiveKey extends DirectiveKey {
  def valueDic: DirectiveDictionary[Value]
}

trait ArbitraryDirectiveKey extends DirectiveKey {
  def valueMapper: String => Value
}

object DirectiveKey {
  def predefined[V <: DirectiveValue](value: String)(implicit dic: DirectiveDictionary[V]) = new PredefinedDirectiveKey {
    override val text: String = value
    override type Value = V
    override val valueDic: DirectiveDictionary[V] = dic
  }

  def arbitrary[V <: DirectiveValue](value: String)(implicit mapper: String => V) = new ArbitraryDirectiveKey {
    override val text: String = value
    override type Value = V
    override val valueMapper = mapper
  }

  lazy val STDLIB_VERSION = DirectiveKey.predefined[StdLibVersion]("STDLIB_VERSION")
  lazy val CONTENT_TYPE   = DirectiveKey.predefined[ContentType]("CONTENT_TYPE")
  lazy val SCRIPT_TYPE    = DirectiveKey.predefined[ScriptType]("SCRIPT_TYPE")
  lazy val IMPORT         = DirectiveKey.arbitrary[Imports]("IMPORT")

  lazy val all: Set[DirectiveKey] =
    Set(
      STDLIB_VERSION,
      CONTENT_TYPE,
      SCRIPT_TYPE,
      IMPORT
    )

  lazy val textMap: Map[String, DirectiveKey] = all.map(d => (d.text, d)).toMap
}
