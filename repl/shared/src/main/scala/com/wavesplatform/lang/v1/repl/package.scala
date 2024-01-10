package com.wavesplatform.lang.v1

import cats.implicits.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.node.ErrorMessageEnvironment
import com.wavesplatform.lang.v1.repl.node.http.{NodeClient, NodeClientImpl, NodeConnectionSettings, WebEnvironment}
import com.wavesplatform.lang.v1.traits.Environment

import scala.concurrent.Future

package object repl {
  val global: BaseGlobal             = com.wavesplatform.lang.Global
  val internalVarPrefixes: Set[Char] = Set('@', '$')
  val internalFuncPrefix: String     = "_"

  val version                  = StdLibVersion.VersionDic.latest
  val directives: DirectiveSet = DirectiveSet(version, Account, DApp).explicitGet()

  val initialCtx: CTX =
    CryptoContext.build(global, version) |+|
      PureContext.build(version, useNewPowPrecision = true) |+|
      WavesContext.build(global, directives, fixBigScriptField = true)

  def buildEnvironment(settings: Option[NodeConnectionSettings], customHttpClient: Option[NodeClient]): Environment[Future] =
    settings.fold(
      ErrorMessageEnvironment[Future]("Blockchain state is unavailable from REPL"): Environment[Future]
    )(s => WebEnvironment(s, customHttpClient.getOrElse(NodeClientImpl(s.normalizedUrl))))
}
