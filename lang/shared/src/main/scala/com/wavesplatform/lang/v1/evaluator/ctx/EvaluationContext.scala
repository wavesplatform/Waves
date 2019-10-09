package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import cats.implicits._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.LetLogCallback
import shapeless.{Lens, lens}

case class EvaluationContext[F[_], C](
   environment: C,
   typeDefs : Map[String, FINAL],
   letDefs  : Map[String, LazyVal[F]],
   functions: Map[FunctionHeader, BaseFunction[F, C]]
)

case class LoggedEvaluationContext[F[_], C](l: LetLogCallback[F], ec: EvaluationContext[F, C])

object LoggedEvaluationContext {
  class Lenses[F[_], C] {
    val types: Lens[LoggedEvaluationContext[F, C], Map[String, FINAL]]                      = lens[LoggedEvaluationContext[F, C]] >> 'ec >> 'typeDefs
    val lets: Lens[LoggedEvaluationContext[F, C], Map[String, LazyVal[F]]]                  = lens[LoggedEvaluationContext[F, C]] >> 'ec >> 'letDefs
    val funcs: Lens[LoggedEvaluationContext[F, C], Map[FunctionHeader, BaseFunction[F, _]]] = lens[LoggedEvaluationContext[F, C]] >> 'ec >> 'functions
  }
}

object EvaluationContext {

  val empty = EvaluationContext(???, Map.empty, Map.empty, Map.empty)

  implicit def monoid[F[_], C]: Monoid[EvaluationContext[F, C]] = new Monoid[EvaluationContext[F, C]] {
    override val empty: EvaluationContext[F, C] = EvaluationContext.empty.asInstanceOf[EvaluationContext[F, C]]

    override def combine(x: EvaluationContext[F, C], y: EvaluationContext[F, C]): EvaluationContext[F, C] =
      EvaluationContext(
        environment = x.environment,
        typeDefs = x.typeDefs ++ y.typeDefs,
        letDefs = x.letDefs ++ y.letDefs,
        functions = x.functions ++ y.functions
      )
  }

  def build[F[_], C](
    environment: C,
    typeDefs:    Map[String, FINAL],
    letDefs:     Map[String, LazyVal[F]],
    functions:   Seq[BaseFunction[F, C]]
  ): EvaluationContext[F, C] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(environment, typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }
}
