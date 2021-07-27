package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, NewAssetInfo}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{TestTime, TransactionGen}
import org.scalatest.{Assertion, EitherValues}
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

  private def expr(invoker: KeyPair, fee: Long, issue: Boolean, transfersCount: Int, receiver: Address, sigVerifyCount: Int): ExprScript =
    TestCompiler(V6).compileFreeCall(
      s"""
         | ${(1 to sigVerifyCount).map(i => s"strict r$i = sigVerify(base58'', base58'', base58'')").mkString("\n")}
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
         |   ${if (transfersCount > 0) "," else ""}
         |   ${(1 to transfersCount).map(_ => s"ScriptTransfer(Address(base58'$receiver'), 1, unit)").mkString(",")}
         | ]
       """.stripMargin
    )

  private def verifier(version: StdLibVersion): Script =
    TestCompiler(version).compileExpression(
      s"""
         | match tx {
         |   case _ => true
         | }
       """.stripMargin
    )

  private def dAppVerifier(version: StdLibVersion): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Verifier(tx)
         | func verify() =
         |   match tx {
         |     case _ => true
         |   }
       """.stripMargin
    )

  private def dAppWithNoVerifier(version: StdLibVersion): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = if (true) then throw() else throw()
       """.stripMargin
    )

  private val forbidByTypeVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case _: InvokeExpressionTransaction => false
         |   case _                              => true
         | }
       """.stripMargin
    )

  private val forbidAllVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case _ => false
         | }
       """.stripMargin
    )

  private def scenario(
      enoughFee: Boolean = true,
      issue: Boolean = true,
      verifier: Option[Script] = None,
      sponsor: Boolean = false,
      version: Byte = 1,
      transfersCount: Int = 0,
      sigVerifyCount: Int = 0
  ) =
    for {
      invoker  <- accountGen
      receiver <- accountGen
      fee      <- ciFee(freeCall = enoughFee, nonNftIssue = if (issue) 1 else 0)
      gtx       = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      stx       = SetScriptTransaction.selfSigned(TxVersion.V2, invoker, verifier, fee, ts).explicitGet()
      issueTx   = IssueTransaction.selfSigned(TxVersion.V2, invoker, "name", "", 1000, 1, true, None, fee, ts).explicitGet()
      asset     = IssuedAsset(issueTx.id.value())
      sponsorTx = SponsorFeeTransaction.selfSigned(TxVersion.V2, invoker, asset, Some(1000L), fee, ts).explicitGet()
      call      = expr(invoker, fee, issue, transfersCount, receiver.toAddress, sigVerifyCount)
      feeAsset  = if (sponsor) asset else Waves
      invoke    = InvokeExpressionTransaction.selfSigned(version, invoker, call, fee, feeAsset, ts).explicitGet()
    } yield (Seq(gtx, issueTx, sponsorTx, stx), invoke)

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

  property("forbid for old version verifier") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ < V6)
      .foreach { v =>
        assertLowVersion(v)
        if (v >= V3) {
          assertLowVersion(v, dApp = true)
          assertNoVerifierNoError(v)
        }
      }

    def assertLowVersion(v: StdLibVersion, dApp: Boolean = false): Assertion = {
      val (genesisTxs, invoke) = scenario(verifier = Some(if (dApp) dAppVerifier(v) else verifier(v))).sample.get
      withDomain(RideV6) { d =>
        d.appendBlock(genesisTxs: _*)
        intercept[Exception](d.appendBlock(invoke)).getMessage should include(
          s"Can't process InvokeExpressionTransaction from RIDE $v verifier, it might be used from V6"
        )
      }
    }

    def assertNoVerifierNoError(v: StdLibVersion): Assertion = {
      val (genesisTxs, invoke) = scenario(verifier = Some(dAppWithNoVerifier(v))).sample.get
      withDomain(RideV6) { d =>
        d.appendBlock(genesisTxs: _*)
        d.appendBlock(invoke)
        d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
      }
    }
  }

  property("allow for V6 verifier") {
    val (genesisTxs, invoke) = scenario(verifier = Some(verifier(V6))).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionInfo(invoke.txId).get._3 shouldBe true
    }
  }

  property("disallow by V6 verifier by type") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidByTypeVerifier)).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("disallow by V6 verifier rejecting all") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidAllVerifier)).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario().sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Ride V6 feature has not been activated yet")
    }
  }

  property("available versions") {
    val unsupportedVersion   = InvokeExpressionTransaction.supportedVersions.max + 1
    val (genesisTxs, invoke) = scenario(version = unsupportedVersion.toByte).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Invalid tx version")
    }
  }

  property("sponsor fee") {
    val (genesisTxs, invoke) = scenario(sponsor = true).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
    }
  }

  property("issue with 29 transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = 29).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
    }
  }

  property("issue with 30 transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = 30).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.txId).get.text shouldBe "Actions count limit is exceeded"
    }
  }

  property("complexity limit") {
    val (genesisTxs, invoke) = scenario(sigVerifyCount = 50).sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Contract function (default) is too complex: 10413 > 10000")
    }
  }
}
