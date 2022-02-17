package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers

class GenesisTransactionDiffTest extends PropSpec with WithState {

  property("fails if height != 1") {
    val genesis = TxHelpers.genesis(TxHelpers.address(1))
    val height = 2
    GenesisTransactionDiff(height)(genesis) should produce("GenesisTransaction cannot appear in non-initial block")
  }

  property("Diff establishes Waves invariant") {
    val genesis = (1 to 10).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 10000))

    assertDiffAndState(Seq.empty, TestBlock.create(genesis)) { (blockDiff, _) =>
      val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
      totalPortfolioDiff.balance shouldBe genesis.map(_.amount).sum
      totalPortfolioDiff.effectiveBalance shouldBe genesis.map(_.amount).sum
      totalPortfolioDiff.assets shouldBe Map.empty

      genesis.foreach { gtx =>
        blockDiff.portfolios(gtx.recipient).balance shouldBe gtx.amount
      }
    }
  }
}
