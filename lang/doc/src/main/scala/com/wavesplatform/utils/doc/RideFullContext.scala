package com.wavesplatform.utils.doc

import cats.implicits._
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment

object RideFullContext {
  def build(ds: DirectiveSet): CTX[Environment] = {
    val wavesCtx  = WavesContext.build(Global,ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx = PureContext.build(ds.stdLibVersion).withEnvironment[Environment]
    pureCtx |+| cryptoCtx |+| wavesCtx
  }
}
