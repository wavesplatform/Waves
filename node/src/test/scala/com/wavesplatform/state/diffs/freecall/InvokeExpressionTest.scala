package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.TransactionGen
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
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxVersion}
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

  private def expr(
      invoker: KeyPair,
      fee: Long,
      issue: Boolean,
      transfersCount: Int,
      receiver: Address,
      sigVerifyCount: Int,
      raiseError: Boolean
  ): ExprScript =
    TestCompiler(V6).compileFreeCall(
      s"""
         | ${(1 to sigVerifyCount).map(i => s"strict r$i = sigVerify(base58'', base58'', base58'')").mkString("\n")}
         | ${if (raiseError) "strict f = throw()" else ""}
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

  private val bigVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | strict r = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" && ")}
         | true
       """.stripMargin
    )

  private def scenario(
      enoughFee: Boolean = true,
      issue: Boolean = true,
      verifier: Option[Script] = None,
      sponsor: Boolean = false,
      version: Byte = 1,
      transfersCount: Int = 0,
      sigVerifyCount: Int = 0,
      bigVerifier: Boolean = false,
      raiseError: Boolean = false
  ): (Seq[Transaction], InvokeExpressionTransaction) = {
    val invoker  = accountGen.sample.get
    val receiver = accountGen.sample.get
    val fee      = ciFee(freeCall = enoughFee, nonNftIssue = if (issue) 1 else 0, sc = if (bigVerifier) 1 else 0).sample.get

    val genesis     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
    val setVerifier = SetScriptTransaction.selfSigned(TxVersion.V2, invoker, verifier, fee, ts).explicitGet()

    val sponsorIssueTx = IssueTransaction.selfSigned(TxVersion.V2, invoker, "name", "", 1000, 1, true, None, fee, ts).explicitGet()
    val sponsorAsset   = IssuedAsset(sponsorIssueTx.id.value())
    val sponsorTx      = SponsorFeeTransaction.selfSigned(TxVersion.V2, invoker, sponsorAsset, Some(1000L), fee, ts).explicitGet()
    val feeAsset       = if (sponsor) sponsorAsset else Waves

    val call   = expr(invoker, fee, issue, transfersCount, receiver.toAddress, sigVerifyCount, raiseError)
    val invoke = InvokeExpressionTransaction.selfSigned(version, invoker, call, fee, feeAsset, ts).explicitGet()

    (Seq(genesis, sponsorIssueTx, sponsorTx, setVerifier), invoke)
  }

  private def feeErrorMessage(invoke: InvokeExpressionTransaction, issue: Boolean = false, verifier: Boolean = false) = {
    val expectingFee = FeeConstants(invoke.tpe) * FeeUnit + (if (issue) 1 else 0) * MinIssueFee + (if (verifier) 1 else 0) * ScriptExtraFee
    val issueErr     = if (issue) " with 1 assets issued" else ""
    val verifierErr  = if (verifier) " with 1 total scripts invoked" else ""
    s"Fee in WAVES for InvokeExpressionTransaction (${invoke.fee} in WAVES)$issueErr$verifierErr does not exceed minimal value of $expectingFee WAVES."
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
    val (genesisTxs, invoke) = scenario()
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.sender.toAddress, "check").get shouldBe BooleanDataEntry("check", true)
      d.blockchain.accountData(invoke.sender.toAddress, "transactionId").get shouldBe BinaryDataEntry("transactionId", invoke.txId)
      d.liquidDiff.issuedAssets.size shouldBe 1
      checkAsset(invoke, d.liquidDiff.issuedAssets.head._2)
    }
  }

  property("insufficient fee leading to reject") {
    val (genesisTxs, invoke) = scenario(enoughFee = false, issue = false)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include(feeErrorMessage(invoke))
    }
  }

  property("insufficient fee for issue leading to fail") {
    val (genesisTxs, invoke) = scenario(enoughFee = false)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.txId).get.text shouldBe feeErrorMessage(invoke, issue = true)
    }
  }

  property("insufficient fee for big verifier leading to fail") {
    val (genesisTxs, invoke) = scenario(issue = false, verifier = Some(bigVerifier))
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.txId).get.text shouldBe feeErrorMessage(invoke, verifier = true)
    }
  }

  property("big verifier with enough fee") {
    val (genesisTxs, invoke) = scenario(issue = false, verifier = Some(bigVerifier), bigVerifier = true)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
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
      val (genesisTxs, invoke) = scenario(verifier = Some(if (dApp) dAppVerifier(v) else verifier(v)))
      withDomain(RideV6) { d =>
        d.appendBlock(genesisTxs: _*)
        intercept[Exception](d.appendBlock(invoke)).getMessage should include(
          s"Can't process InvokeExpressionTransaction from RIDE $v verifier, it might be used from V6"
        )
      }
    }

    def assertNoVerifierNoError(v: StdLibVersion): Assertion = {
      val (genesisTxs, invoke) = scenario(verifier = Some(dAppWithNoVerifier(v)))
      withDomain(RideV6) { d =>
        d.appendBlock(genesisTxs: _*)
        d.appendBlock(invoke)
        d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
      }
    }
  }

  property("allow for V6 verifier") {
    val (genesisTxs, invoke) = scenario(verifier = Some(verifier(V6)))
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.txId) shouldBe true
    }
  }

  property("disallow by V6 verifier by type") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidByTypeVerifier))
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("disallow by V6 verifier rejecting all") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidAllVerifier))
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario()
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Ride V6 feature has not been activated yet")
    }
  }

  ignore("available versions") { // TODO check is commented in CommonValidation
    val unsupportedVersion   = InvokeExpressionTransaction.supportedVersions.max + 1
    val (genesisTxs, invoke) = scenario(version = unsupportedVersion.toByte)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Invalid tx version")
    }
  }

  property("sponsor fee") {
    val (genesisTxs, invoke) = scenario(sponsor = true)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
    }
  }

  property("issue with 29 transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = 29)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
    }
  }

  property("issue with 30 transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = 30)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.txId).get.text shouldBe "Actions count limit is exceeded"
    }
  }

  property("complexity limit") {
    val (genesisTxs, invoke) = scenario(sigVerifyCount = 50)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Contract function (default) is too complex: 10413 > 10000")
    }
  }

  property("reject due to script error") {
    val (genesisTxs, invoke) = scenario(raiseError = true)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("ScriptExecutionError(error = Explicit script termination")
    }
  }

  property("fail due to script error") {
    val (genesisTxs, invoke) = scenario(raiseError = true, sigVerifyCount = 5)
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs: _*)
      d.appendBlock(invoke)
      d.liquidDiff.errorMessage(invoke.id.value()).get.text should include("Explicit script termination")
    }
  }
}
