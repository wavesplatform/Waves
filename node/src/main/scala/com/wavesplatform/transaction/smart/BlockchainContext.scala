package com.wavesplatform.transaction.smart

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, Global}
import com.wavesplatform.state._
import monix.eval.Coeval

import java.util

object BlockchainContext {

  type In = WavesEnvironment.In

  private[this] val cache = new util.HashMap[(StdLibVersion, Boolean, DirectiveSet), CTX[Environment]]()

  def build(
      version: StdLibVersion,
      nByte: Byte,
      in: Coeval[Environment.InputEntity],
      h: Coeval[Int],
      blockchain: Blockchain,
      isTokenContext: Boolean,
      isContract: Boolean,
      address: Environment.Tthis,
      txId: ByteStr
  ): Either[ExecutionError, EvaluationContext[Environment, Id]] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map { ds =>
      val environment         = new WavesEnvironment(nByte, in, h, blockchain, address, ds, txId)
      val fixUnicodeFunctions = blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)
      build(ds, environment, fixUnicodeFunctions)
    }

  def build(
      ds: DirectiveSet,
      environment: Environment[Id],
      fixUnicodeFunctions: Boolean = true
  ): EvaluationContext[Environment, Id] =
    cache
      .synchronized(
        cache.computeIfAbsent(
          (ds.stdLibVersion, fixUnicodeFunctions, ds), { _ =>
            PureContext.build(ds.stdLibVersion, fixUnicodeFunctions).withEnvironment[Environment] |+|
              CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment] |+|
              WavesContext.build(Global, ds)
          }
        )
      )
      .evaluationContext(environment)
}
