package com.wavesplatform.state.diffs.invoke

import cats.syntax.semigroup.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures.{ConsensusImprovements, SynchronousCalls}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.{Account, DApp, StdLibVersion, V3}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.InvariableContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Functions, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.InvokeFunction

object CachedDAppCTX {
  private val cache: Map[(StdLibVersion, Boolean, Boolean), InvariableContext] =
    (for {
      version            <- DirectiveDictionary[StdLibVersion].all.filter(_ >= V3)
      useNewPowPrecision <- Seq(true, false)
      fixBigScriptField  <- Seq(true, false)
    } yield {
      val ctx = PureContext.build(version, useNewPowPrecision) |+|
        CryptoContext.build(Global, version) |+|
        WavesContext.build(Global, DirectiveSet(version, Account, DApp).explicitGet(), fixBigScriptField) |+|
        CTX(Seq.empty, Map.empty, Array(true, false).map(reentrant => new InvokeFunction(reentrant, Functions.callDAppF(reentrant))))
      ((version, useNewPowPrecision, fixBigScriptField), InvariableContext(ctx))
    }).toMap

  def get(version: StdLibVersion, b: Blockchain): InvariableContext =
    cache(
      (
        version,
        b.isFeatureActivated(SynchronousCalls) && b.height > b.settings.functionalitySettings.enforceTransferValidationAfter,
        b.isFeatureActivated(ConsensusImprovements)
      )
    )
}
