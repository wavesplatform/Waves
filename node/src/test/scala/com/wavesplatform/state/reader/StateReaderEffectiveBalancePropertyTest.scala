package com.wavesplatform.state.reader

import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.TestFunctionalitySettings.Enabled
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.state.diffs._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers

class StateReaderEffectiveBalancePropertyTest extends PropSpec with WithState {

  property("No-interactions genesis account's effectiveBalance doesn't depend on depths") {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    val emptyBlocksAmt = 10
    val confirmations = 20

    val genesisBlock = block(Seq(genesis))
    val nextBlocks   = List.fill(emptyBlocksAmt - 1)(block(Seq.empty))
    assertDiffAndState(genesisBlock +: nextBlocks, block(Seq.empty)) { (_, newState) =>
      newState.effectiveBalance(genesis.recipient, confirmations) shouldBe genesis.amount
    }
  }

  property("Negative generating balance case") {
    val fs  = Enabled.copy(preActivatedFeatures = Map(SmartAccounts.id -> 0, SmartAccountTrading.id -> 0))
    val Fee = 100000
    val setup = {
      val master = TxHelpers.signer(1)
      val leaser = TxHelpers.signer(2)

      val genesis = TxHelpers.genesis(master.toAddress)
      val xfer1 = TxHelpers.transfer(master, leaser.toAddress, ENOUGH_AMT / 3)
      val lease1 = TxHelpers.lease(leaser, master.toAddress, xfer1.amount - Fee, fee = Fee)
      val xfer2 = TxHelpers.transfer(master, leaser.toAddress, ENOUGH_AMT / 3)
      val lease2 = TxHelpers.lease(leaser, master.toAddress, xfer2.amount - Fee, fee = Fee)

      (leaser, genesis, xfer1, lease1, xfer2, lease2)
    }

    val (leaser, genesis, xfer1, lease1, xfer2, lease2) = setup
    assertDiffAndState(Seq(block(Seq(genesis)), block(Seq(xfer1, lease1))), block(Seq(xfer2, lease2)), fs) { (_, state) =>
      val portfolio       = state.wavesPortfolio(lease1.sender.toAddress)
      val expectedBalance = xfer1.amount + xfer2.amount - 2 * Fee
      portfolio.balance shouldBe expectedBalance
      state.generatingBalance(leaser.toAddress, state.lastBlockId) shouldBe 0
      portfolio.lease shouldBe LeaseBalance(0, expectedBalance)
      portfolio.effectiveBalance.explicitGet() shouldBe 0
    }
  }
}
