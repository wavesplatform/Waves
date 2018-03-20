package com.wavesplatform.state2.patch

import com.wavesplatform.state2.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction


class CancelLeaseOverflowTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 5)

  property("CancelLeaseOverflow cancels active outgoing leases for accounts having negative spendable balances") {
    val leaseOverflowGen = for {
      sender1 <- accountGen
      sender2 <- accountGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
    } yield (
      GenesisTransaction.create(sender1, amount + fee, ts).right.get,
      GenesisTransaction.create(sender2, amount + fee * 2, ts).right.get,
      LeaseTransaction.create(sender1, amount, fee, ts, sender2).right.get,
      LeaseTransaction.create(sender2, amount, fee, ts, recipient).right.get,
      TransferTransaction.create(None, sender2, recipient, amount, ts, None, fee, Array.emptyByteArray).right.get
    )

    forAll(leaseOverflowGen) { case (gt1, gt2, lease1, lease2, tx) =>
      assertDiffAndState(Seq(
        TestBlock.create(Seq(gt1, gt2)),
        TestBlock.create(Seq(lease1)),
        TestBlock.create(Seq(lease2, tx)),
        TestBlock.create(Seq.empty)),
        TestBlock.create(Seq.empty),
        settings) { case (_, newState) =>
        newState.leaseDetails(lease2.id()).forall(_.isActive) shouldBe false
        newState.leaseDetails(lease1.id()).exists(_.isActive) shouldBe true
      }
    }
  }
}
