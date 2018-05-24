package com.wavesplatform.state.diffs.smart.scenarios

import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.TypeInfo
import com.wavesplatform.lang.TypeInfo._
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
import scorex.account.AddressScheme
import scorex.transaction.assets.IssueTransactionV2
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._
import scorex.transaction.{DataTransaction, GenesisTransaction}

class HackatonScenartioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
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

      // senderAddress.bytes == company.bytes || (isNotary1Agreed && isRecipientAgreed)
      assetScript = s"""
                    |
                    | let king = extract(addressFromString("${king.address}"))
                    | let company = extract(addressFromString("${company.address}"))
                    | let notary1 = addressFromPublicKey(extract(getByteArray(king,"notary1PK")))
                    | let txIdBase58String = toBase58String(tx.id)
                    | let notary1Agreement = getBoolean(notary1,txIdBase58String)
                    | let isNotary1Agreed = if(isDefined(notary1Agreement)) then extract(notary1Agreement) else false
                    | let recipientAddress = addressFromRecipient(tx.recipient)
                    | let recipientAgreement = getBoolean(recipientAddress,txIdBase58String)
                    | let isRecipientAgreed = if(isDefined(recipientAgreement)) then extract(recipientAgreement) else false
                    | let senderAddress = addressFromPublicKey(tx.senderPk)
                    | senderAddress.bytes == company.bytes || (isNotary1Agreed && isRecipientAgreed)
                    |
                    |
        """.stripMargin

      untypedScript = {
        val r = Parser(assetScript).get.value
        assert(r.size == 1)
        r.head
      }

      typedScript = ScriptV1(CompilerV1(dummyTypeCheckerContext, untypedScript).explicitGet()).explicitGet()

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

  private def eval[T: TypeInfo](code: String) = {
    val untyped = Parser(code).get.value
    assert(untyped.size == 1)
    val typed = CompilerV1(dummyTypeCheckerContext, untyped.head)
    typed.flatMap(EvaluatorV1[T](dummyContext, _))
  }

  property("Script toBase58String") {
    eval[Boolean]("toBase58String(base58'AXiXp5CmwVaq4Tp6h6') == \"AXiXp5CmwVaq4Tp6h6\"").explicitGet() shouldBe true
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
