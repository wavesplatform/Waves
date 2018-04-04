package com.wavesplatform.lang.ctx

import com.wavesplatform.lang.FunctionHeader

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction]) {
  def functionByName(name: String): List[PredefFunction] = functions.filter(_._1.name == name).values.toList
}

object Context {
  val empty = Context(Map.empty, Map.empty, Map.empty)
}
