package com.wavesplatform.transaction.smart

import cats.implicits._
import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, Global}
import com.wavesplatform.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = WavesEnvironment.In
  def build(version: StdLibVersion,
            nByte: Byte,
            in: Coeval[Environment.InputEntity],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean,
            isContract: Boolean,
            address: Coeval[ByteStr],
            txId: ByteStr): Either[ExecutionError, EvaluationContext[Environment, Id]] = {
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map { ds =>
      val ctx =
        PureContext.build(Global, version).withEnvironment[Environment]   |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        WavesContext.build(ds)

      ctx.evaluationContext(new WavesEnvironment(nByte, in, h, blockchain, address, ds, txId))
    }
  }
}
