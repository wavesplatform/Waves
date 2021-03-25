package com.wavesplatform.state.diffs.invoke

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.{Account, DApp, StdLibVersion, V3}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.evaluator.ctx.InvariableContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment

object CachedDAppCTX {
  val forVersion: Map[StdLibVersion, InvariableContext] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .map { version =>
        val ctx = PureContext.build(version).withEnvironment[Environment] |+|
          CryptoContext.build(Global, version).withEnvironment[Environment] |+|
          WavesContext.build(Global, DirectiveSet(version, Account, DApp).explicitGet())

        (version, InvariableContext(ctx))
      }
      .toMap
}
