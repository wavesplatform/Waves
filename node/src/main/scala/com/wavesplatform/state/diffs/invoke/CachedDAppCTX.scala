package com.wavesplatform.state.diffs.invoke

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.{Account, DApp, StdLibVersion, V3}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.evaluator.ctx.InvariableContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.Blockchain

object CachedDAppCTX {
  private val cache: Map[(StdLibVersion, Boolean), InvariableContext] =
    (for {
      version             <- DirectiveDictionary[StdLibVersion].all.filter(_ >= V3)
      fixUnicodeFunctions <- Seq(true, false)
    } yield {
      val ctx = PureContext.build(version, fixUnicodeFunctions).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(version, Account, DApp).explicitGet())
      ((version, fixUnicodeFunctions), InvariableContext(ctx))
    }).toMap

  def get(version: StdLibVersion, blockchain: Blockchain): InvariableContext =
    cache((version, blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)))
}
