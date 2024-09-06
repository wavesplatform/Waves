package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.lang.directives.values.V7
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers.*

class SyncDAppGeneratingBalanceTest extends PropSpec with WithDomain {
  property("sync balance changes should be taken into account for the generatingBalance field") {
    val amount = 777
    val dApp = TestCompiler(V7).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   strict generatingBefore = i.caller.wavesBalance().generating
         |   strict result = Address(base58'$defaultAddress').invoke("call", [], [AttachedPayment(unit, $amount)])
         |   strict generatingAfter = i.caller.wavesBalance().generating
         |   [
         |     IntegerEntry("generatingDiff", generatingBefore - generatingAfter)
         |   ]
         | }
         |
         | @Callable(i)
         | func call() = []
       """.stripMargin
    )
    withDomain(
      BlockRewardDistribution.setFeaturesHeight(LightNode -> 4),
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
    ) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp))

      d.appendAndAssertSucceed(invoke(secondAddress, invoker = secondSigner))
      d.liquidSnapshot.accountData.head._2.head._2.value shouldBe 0

      d.appendAndAssertSucceed(invoke(secondAddress, invoker = secondSigner))
      d.liquidSnapshot.accountData.head._2.head._2.value shouldBe amount
    }
  }
}
