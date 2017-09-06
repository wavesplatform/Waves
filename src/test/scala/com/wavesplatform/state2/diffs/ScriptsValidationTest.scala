package com.wavesplatform.state2.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang._
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB, crypto}
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PublicKeyAccount
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction._
import scorex.transaction.assets.{ScriptTransferTransaction, TransferTransaction}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.{Script, SetScriptTransaction, WavesContext}

class ScriptsValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))

  private val context = TypeChecker.TypeCheckerContext.fromContext(WavesContext.build(Coeval(???), Coeval(???), null))

  def preconditionsTransferAndLease(code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = {
    val untyped = Parser(code).get.value
    val typed   = TypeChecker(context, untyped).explicitGet()
    preconditionsTransferAndLease(typed)
  }

  def preconditionsTransferAndLease(typed: Typed.EXPR): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] =
    for {
      master    <- accountGen
      recepient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setScript <- selfSignedSetScriptTransactionGenP(master, Script(typed))
      transfer  <- transferGeneratorP(master, recepient.toAddress, None, None)
      lease     <- leaseAndCancelGeneratorP(master, recepient.toAddress, master)
    } yield (genesis, setScript, lease._1, transfer)

  property("transfer is allowed but lease is not due to predicate") {
    import Typed._

    val onlySend: EXPR = BINARY_OP(
      BINARY_OP(
        BINARY_OP(GETTER(REF("TX", TYPEREF("Transaction")), "TYPE", INT), EQ_OP, CONST_INT(4), BOOLEAN),
        OR_OP,
        BINARY_OP(GETTER(REF("TX", TYPEREF("Transaction")), "TYPE", INT), EQ_OP, CONST_INT(11), BOOLEAN),
        BOOLEAN
      ),
      AND_OP,
      FUNCTION_CALL(
        "SIGVERIFY",
        List(
          GETTER(REF("TX", TYPEREF("Transaction")), "BODYBYTES", BYTEVECTOR),
          GETTER(REF("TX", TYPEREF("Transaction")), "PROOFA", BYTEVECTOR),
          GETTER(REF("TX", TYPEREF("Transaction")), "SENDERPK", BYTEVECTOR)
        ),
        BOOLEAN
      ),
      BOOLEAN
    )
    forAll(preconditionsTransferAndLease(onlySend)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), fs) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), fs)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

  property("2 of 3 multisig") {
    def multisig2Of3Lang(pk0: PublicKeyAccount, pk1: PublicKeyAccount, pk2: PublicKeyAccount): Typed.EXPR = {
      val script =
        s"""let A = base58'${ByteStr(pk0.publicKey)}'
           |let B = base58'${ByteStr(pk1.publicKey)}'
           |let C = base58'${ByteStr(pk2.publicKey)}'
           |
          |let AC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFA,A)) then 1 else 0
           |let BC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFB,B)) then 1 else 0
           |let CC = if(SIGVERIFY(TX.BODYBYTES,TX.PROOFC,C)) then 1 else 0
           |
           | AC + BC+ CC >= 2""".stripMargin
      val untyped = Parser(script).get.value
      TypeChecker(context, untyped).explicitGet()
    }

    val preconditionsAndTransfer: Gen[(GenesisTransaction, SetScriptTransaction, ScriptTransferTransaction, Seq[ByteStr])] = for {
      master    <- accountGen
      s0        <- accountGen
      s1        <- accountGen
      s2        <- accountGen
      recepient <- accountGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      setSctipt <- selfSignedSetScriptTransactionGenP(master, Script(multisig2Of3Lang(s0, s1, s2)))
      amount    <- positiveLongGen
      fee       <- smallFeeGen
      timestamp <- timestampGen
    } yield {
      val unsigned =
        ScriptTransferTransaction
          .create(1, None, master, recepient, amount, timestamp, fee, Array.emptyByteArray, proofs = Proofs.empty)
          .explicitGet()
      val sig0 = ByteStr(crypto.sign(s0, unsigned.bodyBytes()))
      val sig1 = ByteStr(crypto.sign(s1, unsigned.bodyBytes()))
      val sig2 = ByteStr(crypto.sign(s2, unsigned.bodyBytes()))
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

        validProofs.foreach(tx => assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), fs) { case _ => () })
        invalidProofs.foreach(tx =>
          assertLeft(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(tx)), fs)("TransactionNotAllowedByScript"))
    }
  }

  property("accessing field of transaction without checking its type first results on exception") {

    val goodScript =
      """
        |
        | if (TX.TYPE == 4) then (TX.ASSETID == None) else false
        |
      """.stripMargin

    val badScript =
      """
        |
        | TX.ASSETID == None
        |
      """.stripMargin

    forAll(preconditionsTransferAndLease(goodScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), fs) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), fs)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }

    forAll(preconditionsTransferAndLease(badScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), fs) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), fs) { totalDiffEi =>
          totalDiffEi should produce("doesn't contain asset id")
        }
    }
  }

}
