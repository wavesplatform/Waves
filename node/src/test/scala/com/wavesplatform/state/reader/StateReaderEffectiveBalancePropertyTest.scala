package com.wavesplatform.state.reader

import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lagonaki.mocks.TestBlock.create as block
import com.wavesplatform.settings.TestFunctionalitySettings.Enabled
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.*
import com.wavesplatform.state.{BalanceSnapshot, LeaseBalance}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, transfer}

class StateReaderEffectiveBalancePropertyTest extends PropSpec with WithDomain {
  import DomainPresets.*

  property("No-interactions genesis account's effectiveBalance doesn't depend on depths") {
    val master = TxHelpers.signer(1)

    val genesis = TxHelpers.genesis(master.toAddress)

    val emptyBlocksAmt = 10
    val confirmations  = 20

    val genesisBlock = block(Seq(genesis))
    val nextBlocks   = List.fill(emptyBlocksAmt - 1)(block(Seq.empty))
    assertDiffAndState(genesisBlock +: nextBlocks, block(Seq.empty)) { (_, newState) =>
      newState.effectiveBalance(genesis.recipient, confirmations) shouldBe genesis.amount.value
    }
  }

  property("Negative generating balance case") {
    val fs  = Enabled.copy(preActivatedFeatures = Map(SmartAccounts.id -> 0, SmartAccountTrading.id -> 0))
    val Fee = 100000
    val setup = {
      val master = TxHelpers.signer(1)
      val leaser = TxHelpers.signer(2)

      val genesis = TxHelpers.genesis(master.toAddress)
      val xfer1   = TxHelpers.transfer(master, leaser.toAddress, ENOUGH_AMT / 3)
      val lease1  = TxHelpers.lease(leaser, master.toAddress, xfer1.amount.value - Fee, fee = Fee)
      val xfer2   = TxHelpers.transfer(master, leaser.toAddress, ENOUGH_AMT / 3)
      val lease2  = TxHelpers.lease(leaser, master.toAddress, xfer2.amount.value - Fee, fee = Fee)

      (leaser, genesis, xfer1, lease1, xfer2, lease2)
    }

    val (leaser, genesis, xfer1, lease1, xfer2, lease2) = setup
    assertDiffAndState(Seq(block(Seq(genesis)), block(Seq(xfer1, lease1))), block(Seq(xfer2, lease2)), fs) { (_, state) =>
      val portfolio       = state.wavesPortfolio(lease1.sender.toAddress)
      val expectedBalance = xfer1.amount.value + xfer2.amount.value - 2 * Fee
      portfolio.balance shouldBe expectedBalance
      state.generatingBalance(leaser.toAddress, state.lastBlockId) shouldBe 0
      portfolio.lease shouldBe LeaseBalance(0, expectedBalance)
      portfolio.effectiveBalance(false) shouldBe Right(0)
    }
  }

  property("correct balance snapshots at height = 2") {
    def assert(settings: WavesSettings, fixed: Boolean) =
      withDomain(settings) { d =>
        d.appendBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe List(
          BalanceSnapshot(1, 600000000, 0, 0, false)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe (
          if (fixed)
            List(
              BalanceSnapshot(2, 1199999999, 0, 0, false),
              BalanceSnapshot(1, 599399999, 0, 0, false)
            )
          else
            List(BalanceSnapshot(2, 1199999999, 0, 0, false))
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          BalanceSnapshot(2, 1199999999, 0, 0, false)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe List(
          BalanceSnapshot(3, 1799999998, 0, 0, false),
          BalanceSnapshot(2, 1199399998, 0, 0, false),
          BalanceSnapshot(1, 599399999, 0, 0, false)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          BalanceSnapshot(3, 1799999998, 0, 0, false)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 3, None) shouldBe List(
          BalanceSnapshot(3, 1799999998, 0, 0, false)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe List(
          BalanceSnapshot(4, 2399999997L, 0, 0, false),
          BalanceSnapshot(3, 1799399997, 0, 0, false),
          BalanceSnapshot(2, 1199399998, 0, 0, false),
          BalanceSnapshot(1, 599399999, 0, 0, false)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          BalanceSnapshot(4, 2399999997L, 0, 0, false),
          BalanceSnapshot(3, 1799399997, 0, 0, false),
          BalanceSnapshot(2, 1199399998, 0, 0, false)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 3, None) shouldBe List(
          BalanceSnapshot(4, 2399999997L, 0, 0, false)
        )
      }

    assert(RideV5, fixed = false)
    assert(RideV6, fixed = true)
  }
}
