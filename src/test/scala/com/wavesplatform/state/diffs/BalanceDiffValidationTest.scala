package com.wavesplatform.state.diffs

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.base.{LeaseTxBase, TransferTxBase}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.modern.TxHeader
import scorex.transaction.modern.lease.{LeasePayload, LeaseTx}

class BalanceDiffValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("disallows overflow") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, GenesisTransaction, TransferTxBase, TransferTxBase)] = for {
      master    <- accountGen
      master2   <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts        <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(master, Long.MaxValue - 1, ts).right.get
      gen2: GenesisTransaction = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).right.get
      fee       <- smallFeeGen
      amount    <- Gen.choose(Long.MaxValue / 2, Long.MaxValue - fee - 1)
      transfer1 <- createAnyWavesTransfer(master, recipient, amount, fee, ts)
      transfer2 <- createAnyWavesTransfer(master2, recipient, amount, fee, ts)
    } yield (gen1, gen2, transfer1, transfer2)

    forAll(preconditionsAndPayment) {
      case ((gen1, gen2, transfer1, transfer2)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, transfer1))), TestBlock.create(Seq(transfer2))) { blockDiffEi =>
          blockDiffEi should produce("negative waves balance")
        }
    }
  }

  property("disallows lease overflow") {
    val setup = for {
      master1   <- accountGen
      master2   <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      gen1 = GenesisTransaction.create(master1, Long.MaxValue - 1, ts).right.get
      gen2 = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).right.get
      fee  <- smallFeeGen
      amt1 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      amt2 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      l1 <- Gen.oneOf(
        LeaseTransaction.create(master1, amt1, fee, ts, recipient).right.get,
        LeaseTx
          .selfSigned(
            TxHeader(LeaseTx.typeId, LeaseTx.supportedVersions.head, master1, fee, ts),
            LeasePayload(amt1, recipient)
          )
          .get
      )
      l2 <- Gen.oneOf(
        LeaseTransaction.create(master2, amt2, fee, ts, recipient).right.get,
        LeaseTx
          .selfSigned(
            TxHeader(LeaseTx.typeId, LeaseTx.supportedVersions.head, master2, fee, ts),
            LeasePayload(amt2, recipient)
          )
          .get
      )
    } yield (gen1, gen2, l1, l2)

    forAll(setup) {
      case (gen1, gen2, l1, l2) =>
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, l1))), TestBlock.create(Seq(l2)))(totalDiffEi =>
          totalDiffEi should produce("negative effective balance"))
    }
  }

  val ownLessThatLeaseOut: Gen[(GenesisTransaction, TransferTxBase, LeaseTxBase, LeaseTxBase, TransferTxBase)] = for {
    master <- accountGen
    alice  <- accountGen
    bob    <- accountGen
    cooper <- accountGen
    ts     <- positiveIntGen
    amt    <- positiveLongGen
    fee    <- smallFeeGen
    genesis: GenesisTransaction                 = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    masterTransfersToAlice: TransferTransaction = createWavesTransfer(master, alice, amt, fee, ts).right.get
    (aliceLeasesToBob, _)                   <- anyLeaseCancelLeaseGen(alice, bob, alice) suchThat (_._1.amount < amt)
    (masterLeasesToAlice, _)                <- anyLeaseCancelLeaseGen(master, alice, master) suchThat (_._1.amount > aliceLeasesToBob.amount)
    transferAmt                             <- Gen.choose(amt - fee - aliceLeasesToBob.amount, amt - fee)
    aliceTransfersMoreThanOwnsMinusLeaseOut <- createAnyWavesTransfer(alice, cooper, transferAmt, fee, ts)
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
          totalDiffEi shouldBe 'right
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
          totalDiffEi should produce("leased being more than own")
        }
    }
  }
}
