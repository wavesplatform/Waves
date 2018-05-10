package com.wavesplatform.lang.v1.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader
import shapeless._

case class Context(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction])

object Context {

  object Lenses {
    val types: Lens[Context, Map[String, PredefType]]             = lens[Context] >> 'typeDefs
    val lets: Lens[Context, Map[String, LazyVal]]                 = lens[Context] >> 'letDefs
    val funcs: Lens[Context, Map[FunctionHeader, PredefFunction]] = lens[Context] >> 'functions
  }

  val empty = Context(Map.empty, Map.empty, Map.empty)

  implicit val monoid: Monoid[Context] = new Monoid[Context] {
    override val empty: Context = Context.empty

    override def combine(x: Context, y: Context): Context =
      Context(typeDefs = x.typeDefs ++ y.typeDefs, letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build(types: Seq[PredefType], letDefs: Map[String, LazyVal], functions: Seq[PredefFunction]): Context =
    Context(types.map(t => t.name -> t).toMap, letDefs, functions.map(f => f.header -> f).toMap)

  def functionCosts(xs: Iterable[PredefFunction]): Map[FunctionHeader, Long] =
    xs.map { x =>
      x.header -> x.cost
    }(collection.breakOut)
}
