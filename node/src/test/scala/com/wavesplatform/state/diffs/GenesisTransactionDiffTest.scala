package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, genesis}

class GenesisTransactionDiffTest extends PropSpec with WithDomain {
  property("fails if height != 1") {
    withDomain(RideV6) { d =>
      d.appendAndAssertSucceed(genesis(defaultAddress))
      d.appendBlockE(genesis(defaultAddress)) should produce("GenesisTransaction cannot appear in non-initial block")
    }
  }

  property("Diff establishes Waves invariant") {
    val genesis = (1 to 10).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 10000))

    assertDiffAndState(Seq.empty, TestBlock.create(genesis)) { (blockDiff, _) =>
      val totalPortfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
      totalPortfolio.balance shouldBe genesis.map(_.amount.value).sum
      totalPortfolio.effectiveBalance(false).explicitGet() shouldBe genesis.map(_.amount.value).sum
      totalPortfolio.assets shouldBe Map.empty

      genesis.foreach { gtx =>
        blockDiff.portfolios(gtx.recipient).balance shouldBe gtx.amount.value
      }
    }
  }
}
