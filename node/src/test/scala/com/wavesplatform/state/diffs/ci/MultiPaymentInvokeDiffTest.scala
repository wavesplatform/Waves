package com.wavesplatform.state.diffs.ci

import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MultiPaymentInvokeDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB with Inside {
  property("multi payment with verifier and transfer set") {
    val wavesTransfer = 111
    forAll(multiPaymentPreconditions(
      dApp(V4, transferPaymentAmount = wavesTransfer, _),
      accountVerifierGen(V4),
      verifier(V4, Asset)
    )) { case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, invoker, fee) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id.value)
          .map(IssuedAsset)
          .map(asset => asset -> blockchain.balance(dAppAcc, asset))
          .toMap

        diff.portfolios(dAppAcc).assets  shouldBe assetBalance
        diff.portfolios(dAppAcc).balance shouldBe -wavesTransfer
        diff.portfolios(invoker).balance shouldBe wavesTransfer - fee
      }
    }
  }

  property("multi payment with repeated asset") {
    forAll(multiPaymentPreconditions(
      dApp(V4, transferPaymentAmount = 1, _),
      accountVerifierGen(V4),
      verifier(V4, Asset),
      repeatAdditionalAsset = true
    )) { case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, _, _) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id.value)
          .map(IssuedAsset)
          .map(asset => asset -> blockchain.balance(dAppAcc, asset))
          .toMap

        diff.portfolios(dAppAcc).assets shouldBe assetBalance
      }
    }
  }

  property("multi payment fails if one of attached asset scripts forbids tx") {
    forAll(
      multiPaymentPreconditions(
        dApp(V4, transferPaymentAmount = 1, _),
        accountVerifierGen(V4),
        verifier(V4, Asset),
        additionalAssetScript = Some(Gen.oneOf(
          verifier(V4, Asset, result = "throw()"),
          verifier(V4, Asset, result = "false")
        ))
      )
    ) { case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      )(_ should produce("type = Asset"))
    }
  }

  property("multi payment fails if any script has version lower V4") {
    assertScriptVersionError(
      (v, _) => s"DApp version $v < 4 doesn't support multiple payment attachment",
      dAppVersionGen = V3
    )
    assertScriptVersionError(
      (v, _) => s"Invoker script version $v < 4 doesn't support multiple payment attachment",
      verifierVersionGen = oldVersions
    )
    assertScriptVersionError(
      (v, id) => s"Attached asset script id=$id version $v < 4 doesn't support multiple payment attachment",
      assetsScriptVersionGen = oldVersions
    )
  }

  property("fee proportionality") {
    forAll(multiPaymentPreconditions(
      dApp(V4, transferPaymentAmount = 1, _),
      accountVerifierGen(V4),
      verifier(V4, Asset),
      withLowFee = false
    )) { case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setVerifier, setDApp))),
        TestBlock.create(Seq(ci)),
        features
      ){
        val expectedFee = (0.005 + 0.004 + 0.004 * (ContractLimits.MaxAttachedPaymentAmount - 1)) * Constants.UnitsInWave
        _ should produce(
          s"Fee in WAVES for InvokeScriptTransaction (1 in WAVES) " +
          s"with ${ContractLimits.MaxAttachedPaymentAmount} total scripts invoked " +
          s"does not exceed minimal value of ${expectedFee.toLong} WAVES"
        )
      }
    }
  }

  private def multiPaymentPreconditions(
    dApp: KeyPair => Script,
    verifier: Gen[Script],
    commonAssetScript: Script,
    additionalAssetScript: Option[Gen[Script]] = None,
    repeatAdditionalAsset: Boolean = false,
    withLowFee: Boolean = true
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, SetScriptTransaction, InvokeScriptTransaction, List[IssueTransaction], KeyPair, KeyPair, Long)] =
    for {
      master        <- accountGen
      invoker       <- accountGen
      ts            <- timestampGen
      fee           <- if (withLowFee) ciFee(ContractLimits.MaxAttachedPaymentAmount) else Gen.const(1L)
      accountScript <- verifier
      commonIssues  <- Gen.listOfN(
        ContractLimits.MaxAttachedPaymentAmount - 1,
        smartIssueTransactionGen(invoker, Gen.const(Some(commonAssetScript)))
      )
      specialIssue <- smartIssueTransactionGen(invoker, additionalAssetScript.fold(Gen.const(none[Script]))(_.map(Some(_))))
    } yield {
      for {
        genesis     <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2    <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setVerifier <- SetScriptTransaction.selfSigned(invoker, Some(accountScript), fee, ts + 2)
        setDApp     <- SetScriptTransaction.selfSigned(master, Some(dApp(invoker)), fee, ts + 2)
        (issues, payments) =
          if (repeatAdditionalAsset) {
            val issues = specialIssue :: commonIssues.drop(1)
            val payments = (specialIssue :: issues).map(i => Payment(1, IssuedAsset(i.id.value)))
            (issues, payments)
          } else {
            val issues = specialIssue :: commonIssues
            val payments = issues.map(i => Payment(1, IssuedAsset(i.id.value)))
            (issues, payments)
          }
        ci <- InvokeScriptTransaction.selfSigned(invoker, master, None, payments, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setVerifier, setDApp, ci, issues, master, invoker, fee)
    }.explicitGet()

  private def assertScriptVersionError(
    message:                (Int, ByteStr) => String,
    dAppVersionGen:         Gen[StdLibVersion] = V4,
    verifierVersionGen:     Gen[StdLibVersion] = V4,
    assetsScriptVersionGen: Gen[StdLibVersion] = V4,
  ): Unit =
    forAll(
      for {
        dAppVersion         <- dAppVersionGen
        verifierVersion     <- verifierVersionGen
        assetsScriptVersion <- assetsScriptVersionGen
        (genesis, setVerifier, setDApp, ci, issues, _, _, _) <- multiPaymentPreconditions(
          dApp(dAppVersion, transferPaymentAmount = 1, _),
          accountVerifierGen(verifierVersion),
          verifier(assetsScriptVersion, Asset)
        )
        oldVersion = List(dAppVersion, verifierVersion, assetsScriptVersion).filter(_ < V4).head
        maybeFailedAssetId = issues.find(_.script.nonEmpty).get.id.value
      } yield (genesis, setVerifier, setDApp, ci, issues, oldVersion, maybeFailedAssetId)
    ) { case (genesis, setVerifier, setDApp, ci, issues, oldVersion, maybeFailedAssetId) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        )(_ should produce(message(oldVersion.id, maybeFailedAssetId)))
    }

  private def dApp(version: StdLibVersion, transferPaymentAmount: Int, transferRecipient: KeyPair): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func default() = TransferSet([ScriptTransfer(
         |    Address(base58'${transferRecipient.stringRepr}'),
         |    $transferPaymentAmount,
         |    unit
         | )])
         |
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, version)
    ContractScript(version, contract).explicitGet()
  }

  private def dAppVerifier(version: StdLibVersion): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Verifier(tx)
         | func verify() = true
         |
       """.stripMargin

    val expr = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, version)
    ContractScript(version, contract).explicitGet()
  }

  private def verifier(version: StdLibVersion, scriptType: ScriptType, result: String = "true"): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id}      #-}
         | {-# CONTENT_TYPE   EXPRESSION         #-}
         | {-# SCRIPT_TYPE    ${scriptType.text} #-}
         |
         | $result
         |
       """.stripMargin

    val expr = Parser.parseExpr(script).get.value
    val compiled = compileExpr(expr, version, scriptType)
    ExprScript(version, compiled).explicitGet()
  }

  private def accountVerifierGen(version: StdLibVersion) =
    if (version >= V3)
      Gen.oneOf(verifier(version, Account), dAppVerifier(version))
    else
      Gen.const(verifier(version, Account))

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.MultiPaymentInvokeScript,
    ).map(_.id -> 0).toMap
  )

  private val oldVersions = Gen.oneOf(V1, V2, V3)
}
