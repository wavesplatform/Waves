package com.wavesplatform.lang.ctx

import cats._
import com.wavesplatform.lang.FunctionHeader

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction])

object Context {
  val empty = Context(Map.empty, Map.empty, Map.empty)
  implicit val cm: Monoid[Context] = new Monoid[Context] {
    override def empty: Context = Context.empty

    override def combine(x: Context, y: Context): Context =
      Context(typeDefs = Monoid.combine(x.typeDefs, y.typeDefs),
              letDefs = Monoid.combine(x.letDefs, y.letDefs),
              functions = Monoid.combine(x.functions, y.functions))
  }

  def build(types: Seq[PredefType], letDefs: Map[String, LazyVal], functions: Seq[PredefFunction]): Context =
    Context(types.map(t => t.name -> t).toMap, letDefs, functions.map(f => f.header -> f).toMap)
}
