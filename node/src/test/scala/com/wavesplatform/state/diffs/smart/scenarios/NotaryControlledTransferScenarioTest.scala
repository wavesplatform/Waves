package com.wavesplatform.state.diffs.smart.scenarios

import cats.syntax.either.*
import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Global, Testing}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.smart.*
import com.wavesplatform.state.diffs.smart.predef.chainId
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval

class NotaryControlledTransferScenarioTest extends PropSpec with WithState {

  private val preconditions = {
    val company  = TxHelpers.signer(1)
    val king     = TxHelpers.signer(2)
    val notary   = TxHelpers.signer(3)
    val accountA = TxHelpers.signer(4)
    val accountB = TxHelpers.signer(5)

    val genesis = Seq(company, king, notary, accountA, accountB).map(acc => TxHelpers.genesis(acc.toAddress))

    val assetScript   = s"""
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
    val untypedScript = Parser.parseExpr(assetScript).get.value
    val typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), V1, untypedScript).explicitGet()._1)
      .explicitGet()

    val issue                   = TxHelpers.issue(company, 100, script = Some(typedScript))
    val assetId                 = IssuedAsset(issue.id())
    val kingDataTransaction     = TxHelpers.data(king, Seq(BinaryDataEntry("notary1PK", notary.publicKey)))
    val transferFromCompanyToA  = TxHelpers.transfer(company, accountA.toAddress, 1, assetId)
    val transferFromAToB        = TxHelpers.transfer(accountA, accountB.toAddress, 1, assetId)
    val notaryDataTransaction   = TxHelpers.data(notary, Seq(BooleanDataEntry(transferFromAToB.id().toString, true)))
    val accountBDataTransaction = TxHelpers.data(accountB, Seq(BooleanDataEntry(transferFromAToB.id().toString, true)))

    (
      genesis,
      issue,
      kingDataTransaction,
      transferFromCompanyToA,
      notaryDataTransaction,
      accountBDataTransaction,
      transferFromAToB
    )
  }

  private val dummyEvalContext: EvaluationContext[Environment, Id] = {
    val ds          = DirectiveSet(V1, Asset, Expression).explicitGet()
    val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, null, ds, ByteStr.empty)
    lazyContexts((ds, true, true))().evaluationContext(environment)
  }

  private def eval(code: String) = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), V1, untyped).map(_._1)
    typed.flatMap(r => EvaluatorV1().apply[EVALUATED](dummyEvalContext, r).leftMap(_.message))
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
    val (genesis, issue, kingDataTransaction, transferFromCompanyToA, notaryDataTransaction, accountBDataTransaction, transferFromAToB) =
      preconditions
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
