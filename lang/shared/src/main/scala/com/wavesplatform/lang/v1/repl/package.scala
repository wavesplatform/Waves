package com.wavesplatform.lang.v1

import cats.implicits._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.node.ErrorMessageEnvironment
import com.wavesplatform.lang.v1.repl.node.http.{NodeConnectionSettings, WebEnvironment}
import com.wavesplatform.lang.v1.traits.Environment

import scala.concurrent.Future

package object repl {
  val global: BaseGlobal = com.wavesplatform.lang.Global
  val internalVarPrefixes: Set[Char] = Set('@', '$')
  val internalFuncPrefix: String = "_"

  val version = V4

  val initialCtx: CTX[Environment] =
    CryptoContext.build(global, V4).withEnvironment[Environment] |+|
    PureContext.build(global, V4).withEnvironment[Environment]   |+|
    WavesContext.build(DirectiveSet(V4, Account, DApp, Imports(Nil)).right.get)

  def buildEnvironment(settings: Option[NodeConnectionSettings]): Environment[Future] =
    settings.fold(ErrorMessageEnvironment: Environment[Future])(WebEnvironment)
}
