package com.wavesplatform.state.diffs.smart.scenarios

import cats.syntax.either._
import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
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
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.utils.{EmptyBlockchain, _}
import monix.eval.Coeval

class NotaryControlledTransferScenarioTest extends PropSpec with WithState {
  private val preconditions = for {
    company  <- accountGen
    king     <- accountGen
    notary   <- accountGen
    accountA <- accountGen
    accountB <- accountGen
    ts       <- timestampGen
    genesis1 = GenesisTransaction.create(company.toAddress, ENOUGH_AMT, ts).explicitGet()
    genesis2 = GenesisTransaction.create(king.toAddress, ENOUGH_AMT, ts).explicitGet()
    genesis3 = GenesisTransaction.create(notary.toAddress, ENOUGH_AMT, ts).explicitGet()
    genesis4 = GenesisTransaction.create(accountA.toAddress, ENOUGH_AMT, ts).explicitGet()
    genesis5 = GenesisTransaction.create(accountB.toAddress, ENOUGH_AMT, ts).explicitGet()

    assetScript = s"""
                    |
                    | match tx {
                    |   case ttx: TransferTransaction =>
                    |      let king = Address(base58'${king.toAddress}')
                    |      let company = Address(base58'${company.toAddress}')
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
                    |   case _ => throw()
                    | }
        """.stripMargin

    untypedScript = Parser.parseExpr(assetScript).get.value

    typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1)
      .explicitGet()

    issueTransaction = IssueTransaction(
      TxVersion.V2,
      company.publicKey,
      "name".utf8Bytes,
      "description".utf8Bytes,
      100,
      0,
      false,
      Some(typedScript),
      1000000,
      ts
    ).signWith(company.privateKey)

    assetId = IssuedAsset(issueTransaction.id())

    kingDataTransaction = DataTransaction
      .selfSigned(1.toByte, king, List(BinaryDataEntry("notary1PK", notary.publicKey)), 1000, ts + 1)
      .explicitGet()

    transferFromCompanyToA = TransferTransaction
      .selfSigned(1.toByte, company, accountA.toAddress, assetId, 1, Waves, 1000, ByteStr.empty, ts + 20)
      .explicitGet()

    transferFromAToB = TransferTransaction
      .selfSigned(1.toByte, accountA, accountB.toAddress, assetId, 1, Waves, 1000, ByteStr.empty, ts + 30)
      .explicitGet()

    notaryDataTransaction = DataTransaction
      .selfSigned(1.toByte, notary, List(BooleanDataEntry(transferFromAToB.id().toString, true)), 1000, ts + 4)
      .explicitGet()

    accountBDataTransaction = DataTransaction
      .selfSigned(1.toByte, accountB, List(BooleanDataEntry(transferFromAToB.id().toString, true)), 1000, ts + 5)
      .explicitGet()
  } yield (
    Seq(genesis1, genesis2, genesis3, genesis4, genesis5),
    issueTransaction,
    kingDataTransaction,
    transferFromCompanyToA,
    notaryDataTransaction,
    accountBDataTransaction,
    transferFromAToB
  )

  def dummyEvalContext(version: StdLibVersion): EvaluationContext[Environment, Id] = {
    val ds          = DirectiveSet(V1, Asset, Expression).explicitGet()
    val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, null, ds, ByteStr.empty)
    lazyContexts(ds)().evaluationContext(environment)
  }

  private def eval(code: String) = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untyped).map(_._1)
    typed.flatMap(r => EvaluatorV1().apply[EVALUATED](dummyEvalContext(V1), r).leftMap(_.message))
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
