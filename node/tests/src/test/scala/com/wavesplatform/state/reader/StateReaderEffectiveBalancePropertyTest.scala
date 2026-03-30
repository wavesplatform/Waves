package com.wavesplatform.state.reader

import com.wavesplatform.TestValues
import com.wavesplatform.block.Block.PlainBlockVersion
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lagonaki.mocks.TestBlock.create as block
import com.wavesplatform.settings.TestFunctionalitySettings.Enabled
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.*
import com.wavesplatform.state.{BalanceSnapshot, LeaseBalance, Height}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Transaction, TxHelpers}

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
          bs(Height(1), regularBalance = 600000000)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe (
          if (fixed)
            List(
              bs(Height(2), regularBalance = 1199999999),
              bs(Height(1), regularBalance = 599399999)
            )
          else
            List(bs(Height(2), regularBalance = 1199999999))
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          bs(Height(2), regularBalance = 1199999999)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe List(
          bs(Height(3), regularBalance = 1799999998),
          bs(Height(2), regularBalance = 1199399998),
          bs(Height(1), regularBalance = 599399999)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          bs(Height(3), regularBalance = 1799999998)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 3, None) shouldBe List(
          bs(Height(3), regularBalance = 1799999998)
        )

        d.appendMicroBlock(transfer(amount = 1))
        d.appendKeyBlock()
        d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe List(
          bs(Height(4), regularBalance = 2399999997L),
          bs(Height(3), regularBalance = 1799399997),
          bs(Height(2), regularBalance = 1199399998),
          bs(Height(1), regularBalance = 599399999)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 2, None) shouldBe List(
          bs(Height(4), regularBalance = 2399999997L),
          bs(Height(3), regularBalance = 1799399997),
          bs(Height(2), regularBalance = 1199399998)
        )
        d.blockchain.balanceSnapshots(defaultAddress, 3, None) shouldBe List(
          bs(Height(4), regularBalance = 2399999997L)
        )
      }

    assert(RideV5, fixed = false)
    assert(RideV6, fixed = true)
  }

  property("correct balance snapshots") {
    val transferTx   = transfer(to = signer(1).toAddress, amount = 3.waves, fee = 0.1.waves)
    val leaseTx      = lease(recipient = signer(1).toAddress, amount = 2.waves, fee = 0.1.waves)
    val startBalance = 7.waves

    // 2 txs in 1 a non-genesis block
    val feeReward = (transferTx.fee.value + leaseTx.fee.value) * 2 / 5
    val feeCost   = transferTx.fee.value + leaseTx.fee.value

    withDomain(RideV6, Seq(AddrWithBalance(defaultAddress, startBalance))) { d =>
      d.appendBlock(transferTx, leaseTx)
      d.blockchain.balanceSnapshots(defaultAddress, 1, None) shouldBe Seq(
        bs(
          height = Height(2),
          regularBalance = startBalance + 6.waves + feeReward - feeCost - transferTx.amount.value,
          leaseOut = leaseTx.amount.value
        ),
        bs(
          height = Height(1),
          regularBalance = startBalance
        )
      )
    }

    // 1 tx in each of 2 non-genesis blocks, from = 0..1
    (0 to 1).foreach { from =>
      withDomain(RideV6, Seq(AddrWithBalance(defaultAddress, 7.waves))) { d =>
        d.appendBlock(transferTx)
        d.appendBlock(leaseTx)
        d.blockchain.balanceSnapshots(defaultAddress, from, None) shouldBe Seq(
          bs(
            height = Height(3),
            regularBalance = startBalance + 12.waves + leaseTx.fee.value * 2 / 5 - leaseTx.fee.value - transferTx.amount.value,
            // leaseIn = 0, transfer fee is fully compensated by reward ↑
            leaseOut = leaseTx.amount.value
          ),
          bs(
            height = Height(2),
            regularBalance = startBalance + 6.waves + transferTx.fee.value * 2 / 5 - transferTx.fee.value - transferTx.amount.value
          ),
          bs(
            height = Height(1),
            regularBalance = startBalance
          )
        )
      }
    }
  }

  property("correct balance snapshots with deposits") {
    val account1 = TxHelpers.signer(1)
    val address1 = account1.toAddress

    val account2 = TxHelpers.signer(2)

    val initBalance = ENOUGH_AMT

    val settings = DeterministicFinality.configure(_.copy(generationPeriodLength = 3))
    withDomain(settings, balances = AddrWithBalance.enoughBalances(account1, account2)) { d =>
      def appendBlock(txs: Transaction*): Unit = {
        val block = d.createBlock(txs, strictTime = true, generator = account2, version = PlainBlockVersion)
        d.appender.appendBlock(block)
      }

      appendBlock() // 2

      val generationPeriod1 = d.rocksDBWriter.currentGenerationPeriod.value.next
      appendBlock( // 3
        commitToGeneration(generationPeriodStart = generationPeriod1.start, sender = account1),
        commitToGeneration(generationPeriodStart = generationPeriod1.start, sender = account2)
      )
      appendBlock() // 4

      val generationPeriod2 = generationPeriod1.next
      appendBlock( // 5
        commitToGeneration(generationPeriodStart = generationPeriod2.start, sender = account1),
        commitToGeneration(generationPeriodStart = generationPeriod2.start, sender = account2)
      )
      (6 to 8).foreach(_ => appendBlock()) // 8 in memory

      val inDB = Seq(
        bs(Height(7), regularBalance = initBalance - TestValues.commitToGenerationFee * 2, deposits = 1), // Released the first deposit
        // 6 - Not changed
        bs(Height(5), regularBalance = initBalance - TestValues.commitToGenerationFee * 2, deposits = 2), // CommitToGenerationTransaction
        // 4 - A first block of a new period, not changed
        bs(Height(3), regularBalance = initBalance - TestValues.commitToGenerationFee, deposits = 1), // CommitToGenerationTransaction
        // 2 - Empty block
        bs(Height(1), regularBalance = initBalance) // Genesis
      )

      d.rocksDBWriter.balanceSnapshots(address1, 1, None) shouldBe inDB
      d.blockchain.balanceSnapshots(address1, 1, None) shouldBe
        bs(Height(8), regularBalance = initBalance - TestValues.commitToGenerationFee * 2, deposits = 1) +: // Same as on 7
        inDB
    }
  }

  private def bs(height: Height, regularBalance: Long, leaseIn: Long = 0, leaseOut: Long = 0, deposits: Int = 0): BalanceSnapshot =
    BalanceSnapshot(height, regularBalance, leaseIn, leaseOut, CommitToGenerationTransaction.DepositInWavelets * deposits)
}
