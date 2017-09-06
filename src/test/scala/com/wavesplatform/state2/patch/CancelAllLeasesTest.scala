package com.wavesplatform.state2.patch

import com.wavesplatform.state2.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}


class CancelAllLeasesTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val settings = TestFunctionalitySettings.Enabled.copy(
    resetEffectiveBalancesAtHeight = 5, allowMultipleLeaseCancelTransactionUntilTimestamp = Long.MaxValue / 2)

  property("CancelAllLeases cancels all active leases and its effects including those in the block") {
    val setupAndLeaseInResetBlock: Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseTransaction)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      otherAccount <- accountGen
      otherAccount2 <- accountGen
      ts <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      genesis2: GenesisTransaction = GenesisTransaction.create(otherAccount, ENOUGH_AMT, ts).right.get
      (lease, _) <- leaseAndCancelGeneratorP(master, recipient, master)
      fee2 <- smallFeeGen
      unleaseOther = LeaseCancelTransaction.create(otherAccount, lease.id(), fee2, ts + 1).right.get
      (lease2, _) <- leaseAndCancelGeneratorP(master, otherAccount2, master)
    } yield (genesis, genesis2, lease, unleaseOther, lease2)

    forAll(setupAndLeaseInResetBlock, timestampGen retryUntil (_ < settings.allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOther, lease2), blockTime) =>
        assertDiffAndState(Seq(
          TestBlock.create(blockTime, Seq(genesis, genesis2, lease, unleaseOther)),
          TestBlock.create(Seq.empty),
          TestBlock.create(Seq.empty),
          TestBlock.create(Seq.empty)),
          TestBlock.create(Seq(lease2)),
          settings) { case (_, newState) =>
          newState.activeLeases shouldBe empty
//          newState.accountPortfolios.map(_._2.leaseInfo).foreach(_ shouldBe LeaseInfo.empty)
        }
    }
  }
}
