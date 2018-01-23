package com.wavesplatform.state2.diffs

import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{ScriptTransferTransaction, TransferTransaction}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.transaction.smart.lang.Terms._

class ScriptsValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

    val onlySend: BOOL = AND(
      OR(EQ_INT(TX_FIELD(Type), CONST_INT(4)), EQ_INT(TX_FIELD(Type), CONST_INT(11))),
      SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof_0), TX_FIELD(SenderPk))
    )

    val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = for {
      master <- accountGen
      recepient <- accountGen
      ts <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(onlySend))
      transfer <- transferGeneratorP(master, recepient.toAddress, None, None)
      lease <- leaseAndCancelGeneratorP(master, recepient.toAddress, master)
    } yield (genesis, setSctipt, lease._1, transfer)

    forAll(preconditionsAndTransfer) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer))) { case (totalDiff, newState) => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)))(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

  property("2 of 3 multisig") {
    def multisig2Of3(pk0: PublicKeyAccount, pk1: PublicKeyAccount, pk2: PublicKeyAccount) =
      GE(
        SUM(
          SUM(
            IF(SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof_0), CONST_BYTEVECTOR(ByteVector(pk0.publicKey))), CONST_INT(1), CONST_INT(0)),
              IF(SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof_1), CONST_BYTEVECTOR(ByteVector(pk1.publicKey))), CONST_INT(1), CONST_INT(0))
            ),
            IF(SIG_VERIFY(TX_FIELD(BodyBytes), TX_FIELD(Proof_2), CONST_BYTEVECTOR(ByteVector(pk2.publicKey))), CONST_INT(1), CONST_INT(0))
          ), CONST_INT(2))

    val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, ScriptTransferTransaction, Seq[ByteStr])] = for {
      master <- accountGen
      s0 <- accountGen
      s1 <- accountGen
      s2 <- accountGen
      recepient <- accountGen
      ts <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(multisig2Of3(s0, s1, s2)))
      amount <- positiveLongGen
      fee <- smallFeeGen
      timestamp <- timestampGen
    } yield {
      val unsigned =
        ScriptTransferTransaction.create(1, None, master, recepient, amount, timestamp, fee, Array.emptyByteArray, proofs = Seq.empty).explicitGet()
      val sig0 = ByteStr(EllipticCurveImpl.sign(s0, unsigned.bodyBytes()))
      val sig1 = ByteStr(EllipticCurveImpl.sign(s1, unsigned.bodyBytes()))
      val sig2 = ByteStr(EllipticCurveImpl.sign(s2, unsigned.bodyBytes()))
      (genesis, setSctipt, unsigned, Seq(sig0, sig1, sig2))
    }

    forAll(preconditionsAndTransfer) {
      case ((genesis, script, transfer, sigs)) =>
        val validProofs = Seq(
          transfer.copy(proofs = Seq(sigs(0), sigs(1))),
          transfer.copy(proofs = Seq(ByteStr.empty, sigs(1), sigs(2)))
        )

        val invalidProofs = Seq(
          transfer.copy(proofs = Seq(sigs(0))),
          transfer.copy(proofs = Seq(sigs(1))),
          transfer.copy(proofs = Seq(sigs(1), sigs(0)))
        )

        validProofs.foreach(tx => assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx))) { case (totalDiff, newState) => () })
        invalidProofs.foreach(tx => assertLeft(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)))("TransactionNotAllowedByScript"))
    }
  }
}
