package com.wavesplatform.lang.v1

import cats.arrow.FunctionK
import cats.implicits._
import cats.{Id, ~>}
import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.node.ErrorMessageEnvironment
import com.wavesplatform.lang.v1.repl.node.http.{NodeConnectionSettings, WebEnvironment}
import com.wavesplatform.lang.v1.traits.Environment

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future

package object repl {
  val global: BaseGlobal = com.wavesplatform.lang.Global
  val internalVarPrefixes: Set[Char] = Set('@', '$')
  val internalFuncPrefix: String = "_"

  val id2Future: Id ~> Future = new FunctionK[Id, Future] {
    def apply[A](a: A): Future[A] = Future.successful(a)
  }

  def buildInitialCtx(settings: Option[NodeConnectionSettings]): CTX[Future] = {
    val environment = settings.fold(ErrorMessageEnvironment: Environment[Future])(WebEnvironment)
    CryptoContext.build(global, V3).mapK(id2Future) |+|
    PureContext.build(global, V3).mapK(id2Future)   |+|
    WavesContext.build[Future](contractDirectiveSet, environment)
  }

}
