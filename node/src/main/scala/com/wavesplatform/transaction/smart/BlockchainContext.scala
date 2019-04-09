package com.wavesplatform.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.{ExecutionError, Global}
import com.wavesplatform.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = WavesEnvironment.In
  def build(version: StdLibVersion,
            nByte: Byte,
            in: Coeval[In],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean,
            isContract: Boolean,
            address: Coeval[ByteStr]): Either[ExecutionError, EvaluationContext] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map(WavesContext.build(_, new WavesEnvironment(nByte, in, h, blockchain, address)))
      .map(Seq(PureContext.build(version), CryptoContext.build(Global), _))
      .map(Monoid.combineAll(_))
      .map(_.evaluationContext)
}
