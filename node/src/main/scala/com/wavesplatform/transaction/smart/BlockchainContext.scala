package com.wavesplatform.transaction.smart

import cats.Id
import cats.syntax.semigroup.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.*
import monix.eval.Coeval

import java.util

object BlockchainContext {

  type In = WavesEnvironment.In

  private[this] val cache = new util.HashMap[(StdLibVersion, Boolean, Boolean, Boolean, DirectiveSet), CTX[Environment]]()

  def build(
      version: StdLibVersion,
      nByte: Byte,
      in: Coeval[Environment.InputEntity],
      h: Coeval[Int],
      blockchain: Blockchain,
      isTokenContext: Boolean,
      isContract: Boolean,
      address: Environment.Tthis,
      txId: ByteStr,
      fixUnicodeFunctions: Boolean,
      useNewPowPrecision: Boolean,
      fixBigScriptField: Boolean
  ): Either[String, EvaluationContext[Environment, Id]] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map { ds =>
      val environment = WavesEnvironment(nByte, in, h, blockchain, address, ds, txId)
      build(ds, environment, fixUnicodeFunctions, useNewPowPrecision, fixBigScriptField)
    }

  def build(
      ds: DirectiveSet,
      environment: Environment[Id],
      fixUnicodeFunctions: Boolean,
      useNewPowPrecision: Boolean,
      fixBigScriptField: Boolean
  ): EvaluationContext[Environment, Id] =
    cache
      .synchronized(
        cache.computeIfAbsent(
          (ds.stdLibVersion, fixUnicodeFunctions, useNewPowPrecision, fixBigScriptField, ds),
          { _ =>
            PureContext.build(ds.stdLibVersion, useNewPowPrecision).withEnvironment[Environment] |+|
              CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment] |+|
              WavesContext.build(Global, ds, fixBigScriptField)
          }
        )
      )
      .evaluationContext(environment)
}
