package com.wavesplatform.state.patch

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CancelAllLeasesTest extends PropSpec with PropertyChecks with WithState with TransactionGen with NoShrink {

  private val settings =
    TestFunctionalitySettings.Enabled.copy(resetEffectiveBalancesAtHeight = 5, lastTimeBasedForkParameter = Long.MaxValue / 2)

  property("CancelAllLeases cancels all active leases and its effects including those in the block") {
    val setupAndLeaseInResetBlock: Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseTransaction, Long)] =
      for {
        master        <- accountGen
        recipient     <- accountGen suchThat (_ != master)
        otherAccount  <- accountGen
        otherAccount2 <- accountGen
        ts            <- Gen.choose(0, settings.lastTimeBasedForkParameter)
        genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2: GenesisTransaction = GenesisTransaction.create(otherAccount.toAddress, ENOUGH_AMT, ts).explicitGet()
        (lease, _) <- leaseAndCancelGeneratorP(master, recipient.toAddress, ts)
        fee2       <- smallFeeGen
        unleaseOther = LeaseCancelTransaction.signed(1.toByte, otherAccount.publicKey, lease.id(), fee2, ts + 1, otherAccount.privateKey).explicitGet()
        (lease2, _) <- leaseAndCancelGeneratorP(master, otherAccount2.toAddress, ts)
      } yield (genesis, genesis2, lease, unleaseOther, lease2, ts)

    forAll(setupAndLeaseInResetBlock) {
      case (genesis, genesis2, lease, unleaseOther, lease2, blockTime) =>
        assertDiffAndState(
          Seq(
            TestBlock.create(blockTime, Seq(genesis, genesis2, lease, unleaseOther)),
            TestBlock.create(Seq.empty),
            TestBlock.create(Seq.empty),
            TestBlock.create(Seq.empty)
          ),
          TestBlock.create(Seq(lease2)),
          settings
        ) {
          case (_, newState) =>
            newState.allActiveLeases shouldBe empty
        }
    }
  }
}
