package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Asset, ScriptType, StdLibVersion, V4}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class CallableV4DiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {
  property("reissue and burn actions results state") {
    forAll(paymentPreconditions()) {
      case (genesis, setScript, invoke, issue, master, reissueAmount, burnAmount) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        ) { case (_, blockchain) =>
          val asset = IssuedAsset(issue.id.value)
          val resultAmount = issue.quantity + reissueAmount - burnAmount

          blockchain.assetDescription(asset).get.totalVolume shouldBe resultAmount
          blockchain.balance(master, asset) shouldBe resultAmount
        }
    }
  }

  property("asset script can disallow reissue") {
    val disallowReissueAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case r: ReissueTransaction => false
          |   case _ => true
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(disallowReissueAsset))) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ should produce("TransactionNotAllowedByScript"))
    }
  }

  property("asset script can disallow burn") {
    val disallowBurnAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case r: BurnTransaction => false
          |   case _ => true
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(disallowBurnAsset))) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ should produce("TransactionNotAllowedByScript"))
    }
  }

  property("asset script can allow burn and reissue") {
    val allowBurnAndReissueAsset: Script =
      assetVerifier(
        """
          | match tx {
          |   case t: ReissueTransaction | BurnTransaction => true
          |   case _ => false
          | }
        """.stripMargin
      )

    forAll(paymentPreconditions(Some(allowBurnAndReissueAsset))) {
      case (genesis, setScript, invoke, issue, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        )(_ shouldBe 'right)
    }
  }

  property("complex check") {
    forAll(complexPaymentPreconditions) {
      case (genesis, setScript, invoke, issue, master, invoker, reissueAmount, burnAmount, transferAmount) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis :+ setScript :+ issue)),
          TestBlock.create(Seq(invoke)),
          features
        ) { case (_, blockchain) =>
          val asset = IssuedAsset(issue.id.value)
          val totalResultAmount     = issue.quantity + (reissueAmount - burnAmount) * 2
          val issuerResultAmount    = issue.quantity + (reissueAmount - burnAmount - transferAmount) * 2
          val recipientResultAmount = transferAmount * 2

          blockchain.assetDescription(asset).get.totalVolume shouldBe totalResultAmount
          blockchain.balance(master, asset) shouldBe issuerResultAmount
          blockchain.balance(invoker, asset) shouldBe recipientResultAmount
        }
    }
  }


  private def paymentPreconditions(
    assetScript: Option[Script] = None
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, Long, Long)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      ts      <- timestampGen
      fee     <- ciFee(1)
      issue   <- issueV2TransactionGen(master, Gen.const(assetScript), reissuableParam = Some(true))
      reissueAmount <- positiveLongGen
      burnAmount    <- Gen.choose(0, reissueAmount)
    } yield {
      val dApp = Some(reissueAndBurnDApp(issue.id.value, reissueAmount, burnAmount))
      for {
        genesis  <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(invoker, master, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue, master, reissueAmount, burnAmount)
    }.explicitGet()

  private def complexPaymentPreconditions: Gen[(List[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction, KeyPair, KeyPair, Long, Long, Long)] =
    for {
      master          <- accountGen
      invoker         <- accountGen
      ts              <- timestampGen
      fee             <- ciFee(1)
      startAmount     <- positiveLongGen
      reissueAmount   <- positiveLongGen
      burnAmount      <- Gen.choose(0, reissueAmount)
      transferAmount  <- Gen.choose(0, reissueAmount - burnAmount)
      assetScript = Some(complexAsset(startAmount, reissueAmount, burnAmount, transferAmount))
      issue   <- issueV2TransactionGen(master, Gen.const(assetScript), reissuableParam = Some(true))

    } yield {
      val dApp = Some(dAppWithComplexResult(issue.id.value, invoker.publicKey.toAddress, reissueAmount, burnAmount, transferAmount))
      for {
        genesis  <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setDApp  <- SetScriptTransaction.selfSigned(master, dApp, fee, ts + 2)
        ci       <- InvokeScriptTransaction.selfSigned(invoker, master, None, Nil, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setDApp, ci, issue, master, invoker, reissueAmount, burnAmount, transferAmount)
    }.explicitGet()

  private def complexAsset(
    startAmount: Long,
    reissueAmount: Long,
    burnAmount: Long,
    transferAmount: Long
  ): Script = {
    val reissueAmount1  = startAmount + reissueAmount
    val burnAmount1     = reissueAmount1 - burnAmount
    val transferAmount1 = burnAmount1 - transferAmount
    val reissueAmount2  = transferAmount1 + reissueAmount
    val burnAmount2     = reissueAmount2 - burnAmount
    val transferAmount2 = burnAmount2 - transferAmount
    assetVerifier(
     s"""
        | match tx {
        |   case t: ReissueTransaction  => t.quantity == $reissueAmount  && (this.quantity == $reissueAmount1  || this.quantity == $reissueAmount2)
        |   case t: BurnTransaction     => t.quantity == $burnAmount     && (this.quantity == $burnAmount1     || this.quantity == $burnAmount2)
        |   case t: TransferTransaction => t.amount   == $transferAmount && (this.quantity == $transferAmount1 || this.quantity == $transferAmount2)
        |   case _ => false
        | }
      """.stripMargin
    )
  }

  private def assetVerifier(result: String): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION 4          #-}
         | {-# CONTENT_TYPE   EXPRESSION #-}
         | {-# SCRIPT_TYPE    ASSET      #-}
         |
         | $result
         |
       """.stripMargin

    val expr = Parser.parseExpr(script).get.value
    val compiled = compileExpr(expr, V4, Asset)
    ExprScript(V4, compiled).explicitGet()
  }

  private def reissueAndBurnDApp(assetId: ByteStr, reissueAmount: Long, burnAmount: Long): Script =
    dApp(
      s"""
         | [
         |   Reissue(base58'$assetId', true, $reissueAmount),
         |   Burn(base58'$assetId', $burnAmount)
         | ]
       """.stripMargin
    )

  private def dAppWithComplexResult(
    assetId: ByteStr,
    recipient: Address,
    reissueAmount: Long,
    burnAmount: Long,
    transferAmount: Long
  ): Script =
    dApp(
      s"""
         | [
         |   IntEntry("key1", 1),
         |   BooleanEntry("key2", true),
         |
         |   Reissue(base58'$assetId', true, $reissueAmount),
         |   Burn(base58'$assetId', $burnAmount),
         |   ScriptTransfer(Address(base58'${recipient.bytes}'), $transferAmount, base58'$assetId'),
         |
         |   StringEntry("key3", "str"),
         |   BinaryEntry("key4", base58'$assetId'),
         |
         |   Reissue(base58'$assetId', false, $reissueAmount),
         |   Burn(base58'$assetId', $burnAmount),
         |   ScriptTransfer(Address(base58'${recipient.bytes}'), $transferAmount, base58'$assetId')
         | ]
       """.stripMargin
    )

  private def dApp(result: String): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION 4       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | @Callable(i)
         | func default() =
         | $result
         |
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, V4)
    ContractScript(V4, contract).explicitGet()
  }

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.MultiPaymentInvokeScript,
    ).map(_.id -> 0).toMap
  )
}
