package com.wavesplatform.state2.diffs

import com.wavesplatform.lang.Parser
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, Proofs}
import scorex.transaction.assets.{ScriptTransferTransaction, TransferTransaction}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}
import com.wavesplatform.lang.Terms._

class ScriptsValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

    val onlySend: Expr = AND(
      OR(EQ(GETTER(REF("TX"),"TYPE"), CONST_INT(4)), EQ(GETTER(REF("TX"),"TYPE"), CONST_INT(11))),
      SIG_VERIFY(GETTER(REF("TX"),"BODYBYTES"), GETTER(REF("TX"),"PROOFA"), GETTER(REF("TX"),"SENDERPK"))
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
    def multisig2Of3Lang(pk0: PublicKeyAccount, pk1: PublicKeyAccount, pk2: PublicKeyAccount) : Expr = {
      val script =
        s"""
          |
          |let A = base58'${ByteStr(pk0.publicKey)}'
          |let B = base58'${ByteStr(pk1.publicKey)}'
          |let C = base58'${ByteStr(pk2.publicKey)}'
          |
          |let AC = if(checkSig(TX.BODYBYTES,TX.PROOFA,A)) then 1 else 0
          |let BC = if(checkSig(TX.BODYBYTES,TX.PROOFB,B)) then 1 else 0
          |let CC = if(checkSig(TX.BODYBYTES,TX.PROOFC,C)) then 1 else 0
          |
          | AC + BC+ CC >= 2
          |
      """.stripMargin
      Parser(script).get.value
    }

    val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, ScriptTransferTransaction, Seq[ByteStr])] = for {
      master <- accountGen
      s0 <- accountGen
      s1 <- accountGen
      s2 <- accountGen
      recepient <- accountGen
      ts <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(multisig2Of3Lang(s0, s1, s2)))
      amount <- positiveLongGen
      fee <- smallFeeGen
      timestamp <- timestampGen
    } yield {
      val unsigned =
        ScriptTransferTransaction.create(1, None, master, recepient, amount, timestamp, fee, Array.emptyByteArray, proofs = Proofs.empty).explicitGet()
      val sig0 = ByteStr(EllipticCurveImpl.sign(s0, unsigned.bodyBytes()))
      val sig1 = ByteStr(EllipticCurveImpl.sign(s1, unsigned.bodyBytes()))
      val sig2 = ByteStr(EllipticCurveImpl.sign(s2, unsigned.bodyBytes()))
      (genesis, setSctipt, unsigned, Seq(sig0, sig1, sig2))
    }

    forAll(preconditionsAndTransfer) {
      case ((genesis, script, transfer, sigs)) =>
        val validProofs = Seq(
          transfer.copy(proofs = Proofs.create(Seq(sigs(0), sigs(1))).explicitGet()),
          transfer.copy(proofs = Proofs.create(Seq(ByteStr.empty, sigs(1), sigs(2))).explicitGet())
        )

        val invalidProofs = Seq(
          transfer.copy(proofs = Proofs.create(Seq(sigs(0))).explicitGet()),
          transfer.copy(proofs = Proofs.create(Seq(sigs(1))).explicitGet()),
          transfer.copy(proofs = Proofs.create(Seq(sigs(1), sigs(0))).explicitGet())
        )

        validProofs.foreach(tx =>
          assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx))) { case (totalDiff, newState) => () })
        invalidProofs.foreach(tx =>
          assertLeft(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)))("TransactionNotAllowedByScript"))
    }
  }
}
