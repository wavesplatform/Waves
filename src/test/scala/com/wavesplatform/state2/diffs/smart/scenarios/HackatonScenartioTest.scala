package com.wavesplatform.state2.diffs.smart.scenarios

import java.nio.charset.StandardCharsets

import com.wavesplatform.{NoShrink, TransactionGen}
import com.wavesplatform.lang.{Evaluator, Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.utils._

import scala.reflect.runtime.universe.TypeTag
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.account.AddressScheme
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{DataTransaction, GenesisTransaction}
import scorex.transaction.assets.{SmartIssueTransaction, TransferTransaction}
import scorex.transaction.smart.Script

class HackatonScenartioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val preconditions: Gen[(Seq[GenesisTransaction], SmartIssueTransaction, DataTransaction, TransferTransaction)] =
    for {
      company  <- accountGen
      king     <- accountGen
      notary   <- accountGen
      accountA <- accountGen
      accountB <- accountGen
      ts       <- timestampGen
      genesis1    = GenesisTransaction.create(company, ENOUGH_AMT, ts).explicitGet()
      genesis2    = GenesisTransaction.create(king, ENOUGH_AMT, ts).explicitGet()
      genesis3    = GenesisTransaction.create(notary, ENOUGH_AMT, ts).explicitGet()
      genesis4    = GenesisTransaction.create(accountA, ENOUGH_AMT, ts).explicitGet()
      genesis5    = GenesisTransaction.create(accountB, ENOUGH_AMT, ts).explicitGet()
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
                    |
                    | addressFromPublicKey(tx.senderPk) == company || (isNotary1Agreed && isRecipientAgreed)
                    |
                    |
        """.stripMargin

      untypedScript = Parser(assetScript).get.value
      typedScript   = Script(TypeChecker(dummyTypeCheckerContext, untypedScript).explicitGet())

      issueTransaction = SmartIssueTransaction
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

      kingDataTransaction = DataTransaction
        .selfSigned(1, king, List(BinaryDataEntry("notary1PK", ByteStr(notary.publicKey))), 1000, ts + 1)
        .explicitGet()

      transferFromCompany = TransferTransaction
        .create(Some(issueTransaction.id()), company, accountA, 1, ts + 2, None, 1000, Array.empty)
        .explicitGet()
      // setScript            <- selfSignedSetScriptTransactionGenP(master, Script(typedScript))
      //transferFromScripted <- versionedTransferGenP(master, alice, Proofs.empty)

    } yield (Seq(genesis1, genesis2, genesis3, genesis4, genesis5), issueTransaction, kingDataTransaction, transferFromCompany)

  private def eval[T: TypeTag](code: String) = {
    val untyped = Parser(code).get.value
    val typed   = TypeChecker(dummyTypeCheckerContext, untyped)
    typed.flatMap(Evaluator[T](dummyContext, _))
  }

  property("Script toBase58String") {
    eval[Boolean]("toBase58String(base58'AXiXp5CmwVaq4Tp6h6') == \"AXiXp5CmwVaq4Tp6h6\"").explicitGet() shouldBe true
  }

  property("Scenario") {
    forAll(preconditions) {
      case (genesis, issue, kingDataTransaction, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(genesis)), TestBlock.create(Seq(issue, kingDataTransaction, transfer)), smartEnabledFS) {
          case (_, state) =>
        }
    }
  }

  property("equals on obj") {
    forAll(accountGen) {
      addr =>
        eval[Boolean](
          s"""

extract(addressFromString("${addr.address}")) == extract(addressFromString("${addr.address}"))

           """.stripMargin) shouldBe Right(true)
    }

  }

  /* property("simple oracle value required to transfer") {
    forAll(preconditions) {
      case ((genesis, genesis2, setScript, dataTransaction, transferFromScripted)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, setScript, dataTransaction))),
                           TestBlock.create(Seq(transferFromScripted)),
                           smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, setScript))), TestBlock.create(Seq(transferFromScripted)), smartEnabledFS)(
          totalDiffEi => totalDiffEi should produce("Script execution error"))
    }
  }*/
}
