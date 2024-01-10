package com.wavesplatform.lang.v1.evaluator.ctx

import java.util

import cats.*
import cats.syntax.functor.*
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.LET
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.{LetExecResult, LetLogCallback}
import com.wavesplatform.lang.v1.traits.Environment
import shapeless.{Lens, lens}

case class EvaluationContext[F[_]](
    environment: Environment[F],
    typeDefs: Map[String, FINAL],
    letDefs: Map[String, LazyVal[F]],
    functions: Map[FunctionHeader, BaseFunction]
) {
  def mapK[G[_]: Monad](f: F ~> G): EvaluationContext[G] =
    EvaluationContext(
      environment.asInstanceOf[Environment[G]],
      typeDefs,
      letDefs.view.mapValues(_.mapK(f)).toMap,
      functions
    )
}

trait LoggedEvaluationContext[F[_]] {
  def ec: EvaluationContext[F]
  def log(let: LET, result: LetExecResult[F]): Unit
}

case class EnabledLogEvaluationContext[F[_]: Monad](l: LetLogCallback[F], ec: EvaluationContext[F])
    extends LoggedEvaluationContext[F] {
  val loggedLets: util.IdentityHashMap[LET, Unit]          = new util.IdentityHashMap()
  val loggedErrors: collection.mutable.Set[ExecutionError] = collection.mutable.Set()

  override def log(let: LET, result: LetExecResult[F]): Unit = {
    result.map {
      case Left(err) if !loggedErrors.contains(err) =>
        loggedErrors.addOne(err)
        add(let, result)
      case Left(_) => ()
      case _       => add(let, result)
    }
  }

  private def add(let: LET, result: LetExecResult[F]): Unit = {
//    loggedLets.computeIfAbsent(let, _ => l(let.name)(result))
  }
}

object EnabledLogEvaluationContext {
  class Lenses[F[_]: Monad] {
    val types: Lens[EnabledLogEvaluationContext[F], Map[String, FINAL]] =
      lens[EnabledLogEvaluationContext[F]] >> Symbol("ec") >> Symbol("typeDefs")
    val lets: Lens[EnabledLogEvaluationContext[F], Map[String, LazyVal[F]]] =
      lens[EnabledLogEvaluationContext[F]] >> Symbol("ec") >> Symbol("letDefs")
    val funcs: Lens[EnabledLogEvaluationContext[F], Map[FunctionHeader, BaseFunction]] =
      lens[EnabledLogEvaluationContext[F]] >> Symbol("ec") >> Symbol("functions")
  }
}

case class DisabledLogEvaluationContext[F[_]](ec: EvaluationContext[F]) extends LoggedEvaluationContext[F] {
  override def log(let: LET, result: LetExecResult[F]): Unit = ()
}
