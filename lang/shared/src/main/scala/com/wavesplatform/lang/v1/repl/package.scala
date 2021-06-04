package com.wavesplatform.lang.v1

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
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

  val version = V5
  val directives: DirectiveSet = DirectiveSet(version, Account, DApp).explicitGet()

  val initialCtx: CTX[Environment] =
    CryptoContext.build(global, version).withEnvironment[Environment]  |+|
    PureContext.build(version, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
    WavesContext.build(global, directives)

  def buildEnvironment(settings: Option[NodeConnectionSettings]): Environment[Future] =
    settings.fold(ErrorMessageEnvironment[Future]("Blockchain state is unavailable from REPL"): Environment[Future])(WebEnvironment)
}
