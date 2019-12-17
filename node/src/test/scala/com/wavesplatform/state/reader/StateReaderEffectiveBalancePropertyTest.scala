package com.wavesplatform.state.reader

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.TestFunctionalitySettings.Enabled
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.LeaseTransactionV2
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class StateReaderEffectiveBalancePropertyTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  property("No-interactions genesis account's effectiveBalance doesn't depend on depths") {
    val setup: Gen[(GenesisTransaction, Int, Int, Int)] = for {
      master <- accountGen
      ts     <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      emptyBlocksAmt <- Gen.choose(1, 10)
      atHeight       <- Gen.choose(1, 20)
      confirmations  <- Gen.choose(1, 20)
    } yield (genesis, emptyBlocksAmt, atHeight, confirmations)

    forAll(setup) {
      case (genesis: GenesisTransaction, emptyBlocksAmt, atHeight, confirmations) =>
        val genesisBlock = block(Seq(genesis))
        val nextBlocks   = List.fill(emptyBlocksAmt - 1)(block(Seq.empty))
        assertDiffAndState(genesisBlock +: nextBlocks, block(Seq.empty)) { (_, newState) =>
          newState.effectiveBalance(genesis.recipient, confirmations) shouldBe genesis.amount
        }
    }
  }

  property("Negative generating balance case") {
    val fs  = Enabled.copy(preActivatedFeatures = Map(SmartAccounts.id -> 0, SmartAccountTrading.id -> 0))
    val Fee = 100000
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      leaser <- accountGen
      xfer1  <- transferGeneratorPV2(ts + 1, master, leaser.toAddress, ENOUGH_AMT / 3)
      lease1 = LeaseTransactionV2.signed(leaser, xfer1.amount - Fee, Fee, ts + 2, master.toAddress, leaser).explicitGet()
      xfer2 <- transferGeneratorPV2(ts + 3, master, leaser.toAddress, ENOUGH_AMT / 3)
      lease2 = LeaseTransactionV2.signed(leaser, xfer2.amount - Fee, Fee, ts + 4, master.toAddress, leaser).explicitGet()
    } yield (leaser, genesis, xfer1, lease1, xfer2, lease2)

    forAll(setup) {
      case (leaser, genesis, xfer1, lease1, xfer2, lease2) =>
        assertDiffAndState(Seq(block(Seq(genesis)), block(Seq(xfer1, lease1))), block(Seq(xfer2, lease2)), fs) { (_, state) =>
          val portfolio       = state.wavesPortfolio(lease1.sender)
          val expectedBalance = xfer1.amount + xfer2.amount - 2 * Fee
          portfolio.balance shouldBe expectedBalance
          state.generatingBalance(leaser, state.lastBlockId) shouldBe 0
          portfolio.lease shouldBe LeaseBalance(0, expectedBalance)
          portfolio.effectiveBalance shouldBe 0
        }
    }
  }
}
