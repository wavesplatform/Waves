package com.wavesplatform.lang.ctx

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[String, PredefFunction])

object Context {
  val empty = Context(Map.empty, Map.empty, Map.empty)
}
