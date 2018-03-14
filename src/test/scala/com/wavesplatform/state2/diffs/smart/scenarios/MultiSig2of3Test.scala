package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang._
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB, crypto}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PublicKeyAccount
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction._
import scorex.transaction.assets.VersionedTransferTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction}

class MultiSig2of3Test extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  def multisigTypedExpr(pk0: PublicKeyAccount, pk1: PublicKeyAccount, pk2: PublicKeyAccount): Typed.EXPR = {
    val script =
      s"""
         |
         |let A = base58'${ByteStr(pk0.publicKey)}'
         |let B = base58'${ByteStr(pk1.publicKey)}'
         |let C = base58'${ByteStr(pk2.publicKey)}'
         |
         |let AC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFA,A)) then 1 else 0
         |let BC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFB,B)) then 1 else 0
         |let CC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFC,C)) then 1 else 0
         |
         | AC + BC+ CC >= 2
         |
      """.stripMargin
    val untyped = Parser(script).get.value
    TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
  }

  val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, VersionedTransferTransaction, Seq[ByteStr])] = for {
    master    <- accountGen
    s0        <- accountGen
    s1        <- accountGen
    s2        <- accountGen
    recepient <- accountGen
    ts        <- positiveIntGen
    genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(multisigTypedExpr(s0, s1, s2)))
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
  } yield {
    val unsigned =
      VersionedTransferTransaction
        .create(2, None, master, recepient, amount, timestamp, fee, Array.emptyByteArray, proofs = Proofs.empty)
        .explicitGet()
    val sig0 = ByteStr(crypto.sign(s0, unsigned.bodyBytes()))
    val sig1 = ByteStr(crypto.sign(s1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(s2, unsigned.bodyBytes()))
    (genesis, setSctipt, unsigned, Seq(sig0, sig1, sig2))
  }

  property("2 of 3 multisig") {

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

        validProofs.foreach { tx =>
          assertDiffAndState(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), smartEnabledFS) { case _ => () }
        }
        invalidProofs.foreach { tx =>
          assertLeft(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), smartEnabledFS)("TransactionNotAllowedByScript")
        }
    }
  }

}
