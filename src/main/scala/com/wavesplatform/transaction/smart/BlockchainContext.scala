package com.wavesplatform.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.Version._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = WavesEnvironment.In
  def build(version: Version, nByte: Byte, in: Coeval[In], h: Coeval[Int], blockchain: Blockchain, isTokenContext: Boolean): EvaluationContext = {
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version),
          CryptoContext.build(Global),
          WavesContext.build(version, new WavesEnvironment(nByte, in, h, blockchain), isTokenContext)
        ))
      .evaluationContext
  }

}
