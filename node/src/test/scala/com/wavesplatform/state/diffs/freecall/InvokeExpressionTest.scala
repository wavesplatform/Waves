package com.wavesplatform.state.diffs.freecall

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V5, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation}
import com.wavesplatform.state.{AssetInfo, AssetStaticInfo, AssetVolumeInfo, BinaryDataEntry, BooleanDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.JsonMatchers
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

import scala.util.Try

class InvokeExpressionTest extends PropSpec with ScalaCheckPropertyChecks with WithDomain with EitherValues with JsonMatchers {
  import DomainPresets.{ContinuationTransaction, RideV6}
  import InvokeExpressionTest.*

  property("can use max contract complexity") {
    def sigVerifyList(size: Int): String = (1 to size).map(_ => "sigVerify(base58'', base58'', base58'')").mkString("[", ", ", "]")

    val script = TxHelpers.freeCallScript(s"""
                                             |strict a = ${sigVerifyList(ContractLimits.MaxTotalInvokeComplexity(V6) / 181)}
                                             |[]
                                             |""".stripMargin)

    TxHelpers.estimate(script) shouldBe 51949

    val bigScript = TxHelpers.freeCallScript(s"""
                                                |strict a = ${sigVerifyList(ContractLimits.MaxTotalInvokeComplexity(V6) / 181 + 1)}
                                                |[]
                                                |""".stripMargin)

    withDomain(DomainPresets.ContinuationTransaction) { d =>
      d.helpers.creditWavesToDefaultSigner()
      d.appendAndAssertSucceed(TxHelpers.invokeExpression(script))
      d.appendAndCatchError(TxHelpers.invokeExpression(bigScript)).toString should include(
        "Contract function (default) is too complex: 52130 > 52000"
      )
    }
  }

  property("cannot create transaction objects") {
    val orderConstructor =
      "Order(base58'', base58'', AssetPair(base58'', base58''), Buy, 1, 1, 1, 1, 1, unit, Address(base58''), base58'', base58'', [])"

    val constructors = Seq(
      "GenesisTransaction(1, Address(base58''), base58'', 1, 1, 1)",
      "PaymentTransaction(1, Address(base58''), base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "TransferTransaction(unit, 1, unit, Alias(\"\"), base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "IssueTransaction(1, \"\", \"\", true, 1, unit, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "ReissueTransaction(1, base58'', true, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "BurnTransaction(1, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SetScriptTransaction(unit, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SponsorFeeTransaction(base58'', 5, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "LeaseTransaction(1, Address(base58''), base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "LeaseCancelTransaction(base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "CreateAliasTransaction(\"\", base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      s"ExchangeTransaction($orderConstructor, $orderConstructor, 1, 1, 1, 1, base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "UpdateAssetInfoTransaction(base58'', \"\", \"\", base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "DataTransaction([], base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "MassTransferTransaction(base58'', 1, [], 1, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "SetAssetScriptTransaction(unit, base58'', base58'', 1, 1, 1, Address(base58''), base58'', base58'', [])",
      "InvokeScriptTransaction(Address(base58''), unit, \"\", [], base58'', 1, 1, 1, Address(base58''), base58'', base58'', [], [])"
    )

    for (constructor <- constructors) withClue("\\w+Transaction".r.findFirstIn(constructor).get) {
      val scriptText =
        s"""
           |strict transfer = $constructor
           |true
           |""".stripMargin
      val scriptV5 = Try(TxHelpers.exprScript(V5)(scriptText))
      scriptV5 shouldBe Symbol("success")

      val scriptV6 = scriptV5.get.copy(stdLibVersion = V6, isFreeCall = true)
      intercept[RuntimeException](TxHelpers.exprScript(V6)(scriptText)).toString should include("Can't find a function")

      withDomain(ContinuationTransaction) { d =>
        d.helpers.creditWavesToDefaultSigner()
        d.appendAndCatchError(TxHelpers.invokeExpression(scriptV6)).toString should include regex "function 'User\\(\\w+\\)' not found".r
      }
    }
  }

  property("can call another contract") {
    val dAppScript = TxHelpers.scriptV5("""
                                          |@Callable(i)
                                          |func test() = ([], 123)
                                          |""".stripMargin)
    val dAppAccount = TxHelpers.secondSigner

    val freeCall = TestCompiler(V6).compileFreeCall(s"""strict test = invoke(Address(base58'${dAppAccount.toAddress}'), "test", [], [])
                                                       |if (test == 123) then [] else throw("err")""".stripMargin)

    val invoke = InvokeExpressionTransaction
      .selfSigned(TxVersion.V1, TxHelpers.defaultSigner, freeCall, 1000000L, Waves, System.currentTimeMillis())
      .explicitGet()
    withDomain(ContinuationTransaction) { d =>
      d.helpers.creditWavesToDefaultSigner()
      d.helpers.creditWavesFromDefaultSigner(dAppAccount.toAddress)
      d.helpers.setScript(dAppAccount, dAppScript)
      d.appendAndAssertSucceed(invoke)

      val result = d.commonApi.invokeScriptResult(invoke.id())
      Json.toJson(result) should matchJson("""{
                                             |  "data" : [ ],
                                             |  "transfers" : [ ],
                                             |  "issues" : [ ],
                                             |  "reissues" : [ ],
                                             |  "burns" : [ ],
                                             |  "sponsorFees" : [ ],
                                             |  "leases" : [ ],
                                             |  "leaseCancels" : [ ],
                                             |  "invokes" : [ {
                                             |    "dApp" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                             |    "call" : {
                                             |      "function" : "test",
                                             |      "args" : [ ]
                                             |    },
                                             |    "payment" : [ ],
                                             |    "stateChanges" : {
                                             |      "data" : [ ],
                                             |      "transfers" : [ ],
                                             |      "issues" : [ ],
                                             |      "reissues" : [ ],
                                             |      "burns" : [ ],
                                             |      "sponsorFees" : [ ],
                                             |      "leases" : [ ],
                                             |      "leaseCancels" : [ ],
                                             |      "invokes" : [ ]
                                             |    }
                                             |  } ]
                                             |}""".stripMargin)
    }
  }

  property("successful applying to the state") {
    val (genesisTxs, invoke) = scenario()
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invoke)
      d.blockchain.accountData(invoke.sender.toAddress, "check").get shouldBe BooleanDataEntry("check", true)
      d.blockchain.accountData(invoke.sender.toAddress, "transactionId").get shouldBe BinaryDataEntry("transactionId", invoke.txId)
      d.liquidSnapshot.assetStatics.size shouldBe 1
      checkAsset(invoke, d.liquidSnapshot.assetStatics.head._2, d.liquidSnapshot.assetNamesAndDescriptions.head._2, d.liquidSnapshot.assetVolumes.head._2)
    }
  }

  property("insufficient fee leading to reject") {
    val (genesisTxs, invoke) = scenario(enoughFee = false, issue = false)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include(feeErrorMessage(invoke))
    }
  }

  property("insufficient fee for issue leading to reject") {
    val (genesisTxs, invoke) = scenario(enoughFee = false)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendAndCatchError(invoke).toString should include(feeErrorMessage(invoke, issue = true))
    }
  }

  property("insufficient fee for big verifier leading to reject") {
    val (genesisTxs, invoke) = scenario(issue = false, verifier = Some(bigVerifier))
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendAndCatchError(invoke).toString should include(feeErrorMessage(invoke, verifier = true))
    }
  }

  property("big verifier with enough fee") {
    val (genesisTxs, invoke) = scenario(issue = false, verifier = Some(bigVerifier), bigVerifier = true)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
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
      withDomain(ContinuationTransaction) { d =>
        d.appendBlock(genesisTxs*)
        intercept[Exception](d.appendBlock(invoke)).getMessage should include(
          s"Can't process InvokeExpressionTransaction from RIDE $v verifier, it might be used from V6"
        )
      }
    }

    def assertNoVerifierNoError(v: StdLibVersion): Assertion = {
      val (genesisTxs, invoke) = scenario(verifier = Some(dAppWithNoVerifier(v)))
      withDomain(ContinuationTransaction) { d =>
        d.appendBlock(genesisTxs*)
        d.appendBlock(invoke)
        d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
      }
    }
  }

  property("allow for V6 verifier") {
    val (genesisTxs, invoke) = scenario(verifier = Some(verifier(V6)))
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.txId) shouldBe true
    }
  }

  property("disallow by V6 verifier by type") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidByTypeVerifier))
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("disallow by V6 verifier rejecting all") {
    val (genesisTxs, invoke) = scenario(verifier = Some(forbidAllVerifier))
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("TransactionNotAllowedByScript")
    }
  }

  property("activation") {
    val (genesisTxs, invoke) = scenario()
    withDomain(RideV6) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include(
        s"${BlockchainFeatures.ContinuationTransaction.description} feature has not been activated yet"
      )
    }
  }

  ignore("available versions") { // TODO check is commented in CommonValidation
    val unsupportedVersion   = 4
    val (genesisTxs, invoke) = scenario(version = unsupportedVersion.toByte)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Invalid tx version")
    }
  }

  property("sponsor fee") {
    val (genesisTxs, invoke) = scenario(sponsor = true)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
    }
  }

  property("issue with 99 transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = 99)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
    }
  }

  property(s"issue with ${ContractLimits.MaxBalanceScriptActionsAmountV6} transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = ContractLimits.MaxBalanceScriptActionsAmountV6)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendAndAssertSucceed(invoke)
    }
  }

  property(s"issue with ${ContractLimits.MaxBalanceScriptActionsAmountV6 + 1} transfers") {
    val (genesisTxs, invoke) = scenario(transfersCount = ContractLimits.MaxBalanceScriptActionsAmountV6 + 1)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendAndCatchError(invoke).toString should include("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded")
    }
  }

  property("complexity limit") {
    val (genesisTxs, invoke) = scenario(sigVerifyCount = 300)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendAndCatchError(invoke).toString should include("Contract function (default) is too complex: 54317 > 52000")
    }
  }

  property("reject due to script error") {
    val (genesisTxs, invoke) = scenario(raiseError = true)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      intercept[Exception](d.appendBlock(invoke)).getMessage should include("Explicit script termination")
    }
  }

  property("fail due to script error") {
    val (genesisTxs, invoke) = scenario(raiseError = true, sigVerifyCount = 6)
    withDomain(ContinuationTransaction) { d =>
      d.appendBlock(genesisTxs*)
      d.appendBlock(invoke)
      d.liquidSnapshot.errorMessage(invoke.id.value()).get.text should include("Explicit script termination")
    }
  }

  private[this] def checkAsset(
      invoke: InvokeExpressionTransaction,
      static: AssetStaticInfo,
      info: AssetInfo,
      volume: AssetVolumeInfo
  ): Assertion = {
    info.name.toStringUtf8 shouldBe TestAssetName
    info.description.toStringUtf8 shouldBe TestAssetDesc
    volume.volume shouldBe TestAssetVolume
    volume.isReissuable shouldBe TestAssetReissuable
    static.decimals shouldBe TestAssetDecimals
    static.nft shouldBe false
    static.issuer shouldBe invoke.sender
  }
}

private object InvokeExpressionTest {
  val TestAssetName       = "name"
  val TestAssetDesc       = "description"
  val TestAssetVolume     = 1000
  val TestAssetDecimals   = 4
  val TestAssetReissuable = true

  def makeExpression(
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
         |   ${if (issue) s""", Issue("$TestAssetName", "$TestAssetDesc", $TestAssetVolume, $TestAssetDecimals, $TestAssetReissuable, unit, 0) """
      else ""}
         |   ${if (transfersCount > 0) "," else ""}
         |   ${(1 to transfersCount).map(_ => s"ScriptTransfer(Address(base58'$receiver'), 1, unit)").mkString(",")}
         | ]
       """.stripMargin
    )

  def scenario(
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
    val invoker  = TxHelpers.signer(1)
    val receiver = TxHelpers.signer(2)
    val fee      = ciFee(freeCall = enoughFee, nonNftIssue = if (issue) 1 else 0, sc = if (bigVerifier) 1 else 0).sample.get

    val genesis     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, TxHelpers.timestamp).explicitGet()
    val setVerifier = SetScriptTransaction.selfSigned(TxVersion.V2, invoker, verifier, fee, TxHelpers.timestamp).explicitGet()

    val sponsorIssueTx =
      IssueTransaction.selfSigned(TxVersion.V2, invoker, "name", "", 1000, 1, true, None, 1.waves, TxHelpers.timestamp).explicitGet()
    val sponsorAsset = IssuedAsset(sponsorIssueTx.id.value())
    val sponsorTx    = SponsorFeeTransaction.selfSigned(TxVersion.V2, invoker, sponsorAsset, Some(1000L), fee, TxHelpers.timestamp).explicitGet()
    val feeAsset     = if (sponsor) sponsorAsset else Waves

    val call   = makeExpression(invoker, fee, issue, transfersCount, receiver.toAddress, sigVerifyCount, raiseError)
    val invoke = InvokeExpressionTransaction.selfSigned(version, invoker, call, fee, feeAsset, TxHelpers.timestamp).explicitGet()

    (Seq(genesis, sponsorIssueTx, sponsorTx, setVerifier), invoke)
  }

  def feeErrorMessage(invoke: InvokeExpressionTransaction, issue: Boolean = false, verifier: Boolean = false): String = {
    val expectingFee =
      FeeConstants(invoke.tpe) * FeeUnit + (if (issue) 1 else 0) * 1_0000_0000L + (if (verifier) 1 else 0) * FeeValidation.ScriptExtraFee
    val issueErr = if (issue) " with 1 assets issued" else ""
    s"for InvokeExpressionTransaction (${invoke.fee} in WAVES)$issueErr does not exceed minimal value of $expectingFee WAVES."
  }

  def verifier(version: StdLibVersion): Script =
    TestCompiler(version).compileExpression(
      s"""
         | match tx {
         |   case _ => true
         | }
       """.stripMargin
    )

  def dAppVerifier(version: StdLibVersion): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Verifier(tx)
         | func verify() =
         |   match tx {
         |     case _ => true
         |   }
       """.stripMargin
    )

  def dAppWithNoVerifier(version: StdLibVersion): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = if (true) then throw() else throw()
       """.stripMargin
    )

  def forbidByTypeVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case _: InvokeExpressionTransaction => false
         |   case _                              => true
         | }
       """.stripMargin
    )

  def forbidAllVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case _ => false
         | }
       """.stripMargin
    )

  def bigVerifier: Script =
    TestCompiler(V6).compileExpression(
      s"""
         | strict r = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" && ")}
         | true
       """.stripMargin
    )
}
