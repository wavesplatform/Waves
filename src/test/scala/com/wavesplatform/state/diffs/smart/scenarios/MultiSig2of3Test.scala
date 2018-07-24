package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer._

class MultiSig2of3Test extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def multisigTypedExpr(pk0: PublicKeyAccount, pk1: PublicKeyAccount, pk2: PublicKeyAccount): EXPR = {
    val script =
      s"""
         |
         |let A = base58'${ByteStr(pk0.publicKey)}'
         |let B = base58'${ByteStr(pk1.publicKey)}'
         |let C = base58'${ByteStr(pk2.publicKey)}'
         |
         |match input {
         |  case _: Order => false
         |  case tx: Transaction       => {
         |    let proofs = tx.proofs
         |    let AC     = if(sigVerify(tx.bodyBytes,proofs[0],A)) then 1 else 0
         |    let BC     = if(sigVerify(tx.bodyBytes,proofs[1],B)) then 1 else 0
         |    let CC     = if(sigVerify(tx.bodyBytes,proofs[2],C)) then 1 else 0
         |
         |    AC + BC+ CC >= 2
         |  }
         |}
         |
      """.stripMargin
    val untyped = Parser(script).get.value
    assert(untyped.size == 1)
    CompilerV1(dummyCompilerContext, untyped.head).explicitGet()._1
  }

  val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, TransferTransactionV2, Seq[ByteStr])] = for {
    version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
    master    <- accountGen
    s0        <- accountGen
    s1        <- accountGen
    s2        <- accountGen
    recepient <- accountGen
    ts        <- positiveIntGen
    genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    setSctipt <- selfSignedSetScriptTransactionGenP(master, ScriptV1(multisigTypedExpr(s0, s1, s2)).explicitGet())
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
  } yield {
    val unsigned =
      TransferTransactionV2
        .create(version, None, master, recepient, amount, timestamp, None, fee, Array.emptyByteArray, proofs = Proofs.empty)
        .explicitGet()
    val sig0 = ByteStr(crypto.sign(s0, unsigned.bodyBytes()))
    val sig1 = ByteStr(crypto.sign(s1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(s2, unsigned.bodyBytes()))
    (genesis, setSctipt, unsigned, Seq(sig0, sig1, sig2))
  }

  property("2 of 3 multisig") {

    forAll(preconditionsAndTransfer) {
      case (genesis, script, transfer, sigs) =>
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
          assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), smartEnabledFS) { case _ => () }
        }
        invalidProofs.foreach { tx =>
          assertLeft(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), smartEnabledFS)("TransactionNotAllowedByScript")
        }
    }
  }

}
