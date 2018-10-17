package com.wavesplatform.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object BlockchainContext {

  type In = Transaction :+: Order :+: CNil
  def build(version: Byte, nByte: Byte, in: Coeval[In], h: Coeval[Int], blockchain: Blockchain): EvaluationContext = {
//    val typeVarsEnabled =///
//      blockchain.activatedFeatures
//        .contains(BlockchainFeatures.SmartAccountTrading.id)

    Monoid
      .combineAll(
        Seq(
          PureContext.ctx, ///build
          CryptoContext.build(Global),
          WavesContext.build(version, new WavesEnvironment(nByte, in, h, blockchain))
        ))
      .evaluationContext
  }
}
