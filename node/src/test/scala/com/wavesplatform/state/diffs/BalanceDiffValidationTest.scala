package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen

class BalanceDiffValidationTest extends PropSpec with WithState {

  val ownLessThatLeaseOut: Gen[(GenesisTransaction, TransferTransaction, LeaseTransaction, LeaseTransaction, TransferTransaction)] = for {
    master <- accountGen
    alice  <- accountGen
    bob    <- accountGen
    cooper <- accountGen
    ts     <- positiveIntGen
    amt    <- positiveLongGen
    fee    <- smallFeeGen
    genesis: GenesisTransaction                 = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    masterTransfersToAlice: TransferTransaction = createWavesTransfer(master, alice.toAddress, amt, fee, ts).explicitGet()
    (aliceLeasesToBob, _)    <- leaseAndCancelGeneratorP(alice, bob.toAddress) suchThat (_._1.amount < amt)
    (masterLeasesToAlice, _) <- leaseAndCancelGeneratorP(master, alice.toAddress) suchThat (_._1.amount > aliceLeasesToBob.amount)
    transferAmt              <- Gen.choose(amt - fee - aliceLeasesToBob.amount, amt - fee)
    aliceTransfersMoreThanOwnsMinusLeaseOut = createWavesTransfer(alice, cooper.toAddress, transferAmt, fee, ts).explicitGet()

  } yield (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut)

  property("can transfer more than own-leaseOut before allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    forAll(ownLessThatLeaseOut) {
      case (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) =>
        assertDiffEi(
          Seq(TestBlock.create(Seq(genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))),
          TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings
        ) { totalDiffEi =>
          totalDiffEi.explicitGet()
        }
    }
  }

  property("cannot transfer more than own-leaseOut after allow-leased-balance-transfer-until") {
    val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 4)

    forAll(ownLessThatLeaseOut) {
      case (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut) =>
        assertDiffEi(
          Seq(
            TestBlock.create(Seq(genesis)),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq()),
            TestBlock.create(Seq(masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))
          ),
          TestBlock.create(Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings
        ) { totalDiffEi =>
          totalDiffEi should produce("trying to spend leased money")
        }
    }
  }
}
