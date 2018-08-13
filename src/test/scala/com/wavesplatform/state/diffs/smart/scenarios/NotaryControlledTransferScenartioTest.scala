package com.wavesplatform.state.diffs.smart.scenarios

import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.utils._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction}

class NotaryControlledTransferScenartioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[
    (Seq[GenesisTransaction], IssueTransactionV2, DataTransaction, TransferTransactionV1, DataTransaction, DataTransaction, TransferTransactionV1)] =
    for {
      company  <- accountGen
      king     <- accountGen
      notary   <- accountGen
      accountA <- accountGen
      accountB <- accountGen
      ts       <- timestampGen
      genesis1 = GenesisTransaction.create(company, ENOUGH_AMT, ts).explicitGet()
      genesis2 = GenesisTransaction.create(king, ENOUGH_AMT, ts).explicitGet()
      genesis3 = GenesisTransaction.create(notary, ENOUGH_AMT, ts).explicitGet()
      genesis4 = GenesisTransaction.create(accountA, ENOUGH_AMT, ts).explicitGet()
      genesis5 = GenesisTransaction.create(accountB, ENOUGH_AMT, ts).explicitGet()

      assetScript = s"""
                    |
                    | match tx {
                    |   case ttx: TransferTransaction =>
                    |      let king = Address(base58'${king.address}')
                    |      let company = Address(base58'${company.address}')
                    |      let notary1 = addressFromPublicKey(extract(getBinary(king, "notary1PK")))
                    |      let txIdBase58String = toBase58String(ttx.id)
                    |      let isNotary1Agreed = match getBoolean(notary1,txIdBase58String) {
                    |        case b : Boolean => b
                    |        case _ : Unit => false
                    |      }
                    |      let recipientAddress = addressFromRecipient(ttx.recipient)
                    |      let recipientAgreement = getBoolean(recipientAddress,txIdBase58String)
                    |      let isRecipientAgreed = if(isDefined(recipientAgreement)) then extract(recipientAgreement) else false
                    |      let senderAddress = addressFromPublicKey(ttx.senderPublicKey)
                    |      senderAddress.bytes == company.bytes || (isNotary1Agreed && isRecipientAgreed)
                    |   case other => throw
                    | }
        """.stripMargin

      untypedScript = {
        val r = Parser(assetScript).get.value
        assert(r.size == 1)
        r.head
      }

      typedScript = ScriptV1(CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1).explicitGet()

      issueTransaction = IssueTransactionV2
        .selfSigned(
          2,
          AddressScheme.current.chainId,
          company,
          "name".getBytes(StandardCharsets.UTF_8),
          "description".getBytes(StandardCharsets.UTF_8),
          100,
          0,
          false,
          Some(typedScript),
          1000000,
          ts
        )
        .explicitGet()

      assetId = issueTransaction.id()

      kingDataTransaction = DataTransaction
        .selfSigned(1, king, List(BinaryDataEntry("notary1PK", ByteStr(notary.publicKey))), 1000, ts + 1)
        .explicitGet()

      transferFromCompanyToA = TransferTransactionV1
        .selfSigned(Some(assetId), company, accountA, 1, ts + 20, None, 1000, Array.empty)
        .explicitGet()

      transferFromAToB = TransferTransactionV1
        .selfSigned(Some(assetId), accountA, accountB, 1, ts + 30, None, 1000, Array.empty)
        .explicitGet()

      notaryDataTransaction = DataTransaction
        .selfSigned(1, notary, List(BooleanDataEntry(transferFromAToB.id().base58, true)), 1000, ts + 4)
        .explicitGet()

      accountBDataTransaction = DataTransaction
        .selfSigned(1, accountB, List(BooleanDataEntry(transferFromAToB.id().base58, true)), 1000, ts + 5)
        .explicitGet()
    } yield
      (Seq(genesis1, genesis2, genesis3, genesis4, genesis5),
       issueTransaction,
       kingDataTransaction,
       transferFromCompanyToA,
       notaryDataTransaction,
       accountBDataTransaction,
       transferFromAToB)

  private def eval[T](code: String) = {
    val untyped = Parser(code).get.value
    assert(untyped.size == 1)
    val typed = CompilerV1(dummyCompilerContext, untyped.head).map(_._1)
    typed.flatMap(EvaluatorV1[T](dummyEvaluationContext, _)._2)
  }

  property("Script toBase58String") {
    val s = "AXiXp5CmwVaq4Tp6h6"
    eval[Boolean](s"""toBase58String(base58'$s') == \"$s\"""").explicitGet() shouldBe true
  }

  property("Script toBase64String") {
    val s = "Kl0pIkOM3tRikA=="
    eval[Boolean](s"""toBase64String(base64'$s') == \"$s\"""").explicitGet() shouldBe true
  }

  property("addressFromString() returns None when address is too long") {
    import Global.MaxAddressLength
    val longAddress = "A" * (MaxAddressLength + 1)
    eval[Any](s"""addressFromString("$longAddress")""") shouldBe Right(())
  }

  property("Scenario") {
    forAll(preconditions) {
      case (genesis, issue, kingDataTransaction, transferFromCompanyToA, notaryDataTransaction, accountBDataTransaction, transferFromAToB) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(issue, kingDataTransaction, transferFromCompanyToA)).explicitGet()
          append(Seq(transferFromAToB)) should produce("NotAllowedByScript")
          append(Seq(notaryDataTransaction)).explicitGet()
          append(Seq(transferFromAToB)) should produce("NotAllowedByScript") //recipient should accept tx
          append(Seq(accountBDataTransaction)).explicitGet()
          append(Seq(transferFromAToB)).explicitGet()
        }
    }
  }
}
