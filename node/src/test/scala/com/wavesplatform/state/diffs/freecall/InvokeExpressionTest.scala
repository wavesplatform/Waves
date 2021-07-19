package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.{TestTime, TransactionGen}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, NewAssetInfo}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.wavesplatform.{TestTime, TransactionGen}
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokeExpressionTest extends PropSpec with ScalaCheckPropertyChecks with TransactionGen with WithDomain with EitherValues {

  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val assetName         = "name"
  private val assetDescription  = "description"
  private val assetVolume       = 1000
  private val assetDecimals     = 4
  private val assetIsReissuable = true

  private def expr(invoker: KeyPair, fee: Long, issue: Boolean): ExprScript =
    TestCompiler(V6).compileFreeCall(
      s"""
         | let address   = Address(base58'${invoker.toAddress}')
         | let publicKey = base58'${invoker.publicKey}'
         | let check =
         |   this                    == address   &&
         |   i.caller                == address   &&
         |   i.originCaller          == address   &&
         |   i.callerPublicKey       == publicKey &&
         |   i.originCallerPublicKey == publicKey &&
         |   i.fee                   == $fee      &&
         |   i.payments              == []        &&
         |   i.feeAssetId            == unit
         | [
         |   BooleanEntry("check", check),
         |   BinaryEntry("transactionId", i.transactionId)
         |   ${if (issue) s""", Issue("$assetName", "$assetDescription", $assetVolume, $assetDecimals, $assetIsReissuable, unit, 0) """ else ""}
         | ]
       """.stripMargin
    )

  private val verifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case i: InvokeExpressionTransaction => i.expression.size() > 0 && i.feeAssetId == unit
         |   case _                              => false
         | }
       """.stripMargin
    )

  private def scenario(enoughFee: Boolean = true, issue: Boolean = true, withVerifier: Boolean = true) =
    for {
      invoker <- accountGen
      fee     <- ciFee(freeCall = enoughFee, nonNftIssue = if (issue) 1 else 0)
      gtx    = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      script = if (withVerifier) Some(verifier) else None
      stx    = SetScriptTransaction.selfSigned(TxVersion.V2, invoker, script, fee, ts).explicitGet()
      call   = expr(invoker, fee, issue)
      invoke = InvokeExpressionTransaction.selfSigned(TxVersion.V1, invoker, call, fee, Waves, ts).explicitGet()
    } yield (Seq(gtx, stx), invoke)

  private def feeErrorMessage(invoke: InvokeExpressionTransaction, issue: Boolean = false) = {
    val expectingFee = FeeConstants(invoke.typeId) * FeeUnit + (if (issue) 1 else 0) * MinIssueFee
    val issueErr     = if (issue) " with 1 assets issued" else ""
    s"Fee in WAVES for InvokeExpressionTransaction (${invoke.fee} in WAVES)$issueErr does not exceed minimal value of $expectingFee WAVES."
  }

  private def checkAsset(
      invoke: InvokeExpressionTransaction,
      asset: NewAssetInfo
  ): Assertion = {
    asset.dynamic.name.toStringUtf8 shouldBe assetName
    asset.dynamic.description.toStringUtf8 shouldBe assetDescription
    asset.volume.volume shouldBe assetVolume
    asset.volume.isReissuable shouldBe assetIsReissuable
    asset.static.decimals shouldBe assetDecimals
    asset.static.nft shouldBe false
    asset.static.issuer shouldBe invoke.sender
  }

  property("successful applying to the state") {
    val (genesisTxs, invoke) = scenario().sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.senderAddress, "check").get shouldBe BooleanDataEntry("check", true)
      d.blockchain.accountData(invoke.senderAddress, "transactionId").get shouldBe BinaryDataEntry("transactionId", invoke.txId)
      d.liquidDiff.issuedAssets.size shouldBe 1
      checkAsset(invoke, d.liquidDiff.issuedAssets.head._2)
    }
  }

  property("insufficient fee leading to reject") {
    val (genesisTxs, invoke) = scenario(enoughFee = false, issue = false).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include(feeErrorMessage(invoke))
    }
  }

  property("insufficient fee leading to fail") {
    val (genesisTxs, invoke) = scenario(enoughFee = false).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.txId).get.text shouldBe feeErrorMessage(invoke, issue = true)
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario(withVerifier = false).sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Ride V6 feature has not been activated yet")
    }
  }
}
