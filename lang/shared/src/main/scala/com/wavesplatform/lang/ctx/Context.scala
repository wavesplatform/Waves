package com.wavesplatform.lang.ctx

import com.wavesplatform.lang.FunctionHeader

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction])

object Context {
  val empty = Context(Map.empty, Map.empty, Map.empty)
}
