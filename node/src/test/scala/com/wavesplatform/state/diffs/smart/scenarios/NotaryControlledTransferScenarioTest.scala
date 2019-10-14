package com.wavesplatform.state.diffs.smart.scenarios

import java.nio.charset.StandardCharsets

import cats.Id
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Global, Testing}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.state.diffs.smart.predef.chainId
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction}
import com.wavesplatform.utils.EmptyBlockchain
import com.wavesplatform.{NoShrink, TransactionGen}
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class NotaryControlledTransferScenarioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
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
                    |      let king = Address(base58'${king.stringRepr}')
                    |      let company = Address(base58'${company.stringRepr}')
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
                    |   case other => throw()
                    | }
        """.stripMargin

      untypedScript = Parser.parseExpr(assetScript).get.value

      typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1)
        .explicitGet()

      issueTransaction = IssueTransactionV2
        .selfSigned(
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

      assetId = IssuedAsset(issueTransaction.id())

      kingDataTransaction = DataTransaction
        .selfSigned(king, List(BinaryDataEntry("notary1PK", ByteStr(notary.publicKey))), 1000, ts + 1)
        .explicitGet()

      transferFromCompanyToA = TransferTransactionV1
        .selfSigned(assetId, company, accountA, 1, ts + 20, Waves, 1000, Array.empty)
        .explicitGet()

      transferFromAToB = TransferTransactionV1
        .selfSigned(assetId, accountA, accountB, 1, ts + 30, Waves, 1000, Array.empty)
        .explicitGet()

      notaryDataTransaction = DataTransaction
        .selfSigned(notary, List(BooleanDataEntry(transferFromAToB.id().base58, true)), 1000, ts + 4)
        .explicitGet()

      accountBDataTransaction = DataTransaction
        .selfSigned(accountB, List(BooleanDataEntry(transferFromAToB.id().base58, true)), 1000, ts + 5)
        .explicitGet()
    } yield
      (Seq(genesis1, genesis2, genesis3, genesis4, genesis5),
       issueTransaction,
       kingDataTransaction,
       transferFromCompanyToA,
       notaryDataTransaction,
       accountBDataTransaction,
       transferFromAToB)

  private val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, Coeval(null))

  def dummyEvalContext(version: StdLibVersion): EvaluationContext[Environment, Id] =
    lazyContexts(DirectiveSet(V1, Asset, Expression).explicitGet())().evaluationContext(environment)

  private def eval(code: String) = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untyped).map(_._1)
    typed.flatMap(EvaluatorV1().apply[EVALUATED](dummyEvalContext(V1), _))
  }

  property("Script toBase58String") {
    val s = "AXiXp5CmwVaq4Tp6h6"
    eval(s"""toBase58String(base58'$s') == \"$s\"""") shouldBe Testing.evaluated(true)
  }

  property("Script toBase64String") {
    val s = "Kl0pIkOM3tRikA=="
    eval(s"""toBase64String(base64'$s') == \"$s\"""") shouldBe Testing.evaluated(true)
  }

  property("addressFromString() returns None when address is too long") {
    val longAddress = "A" * (Global.MaxBase58String + 1)
    eval(s"""addressFromString("$longAddress")""") shouldBe Left("base58Decode input exceeds 100")
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
