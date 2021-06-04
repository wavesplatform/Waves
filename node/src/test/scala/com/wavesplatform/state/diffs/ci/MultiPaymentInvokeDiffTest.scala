package com.wavesplatform.state.diffs.ci

import cats.implicits._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.Diff
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MultiPaymentInvokeDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithState with Inside {
  private val oldVersions = Gen.oneOf(V1, V2, V3)

  property("multi payment with verifier and transfer set") {
    val wavesTransfer = 111
    forAll(
      paymentPreconditions(
        dApp(V4, transferPaymentAmount = wavesTransfer, _),
        accountVerifierGen(V4),
        verifier(V4, Asset)
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, invoker, fee) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        ) {
          case (diff, blockchain) =>
            val assetBalance = issues
              .map(_.id())
              .map(IssuedAsset)
              .map(asset => asset -> blockchain.balance(dAppAcc.toAddress, asset))
              .filter(_._2 != 0)
              .toMap

            diff.portfolios(dAppAcc.toAddress).assets shouldBe assetBalance
            diff.portfolios(dAppAcc.toAddress).balance shouldBe -wavesTransfer
            diff.portfolios(invoker.toAddress).balance shouldBe wavesTransfer - fee
        }
    }
  }

  property("multi payment with repeated asset") {
    forAll(
      paymentPreconditions(
        dApp(V4, transferPaymentAmount = 1, _),
        accountVerifierGen(V4),
        verifier(V4, Asset),
        repeatAdditionalAsset = true
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, _, _) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        ) {
          case (diff, blockchain) =>
            val assetBalance = issues
              .map(_.id())
              .map(IssuedAsset)
              .map(asset => asset -> blockchain.balance(dAppAcc.toAddress, asset))
              .filter(_._2 != 0)
              .toMap

            diff.portfolios(dAppAcc.toAddress).assets shouldBe assetBalance
        }
    }
  }

  property("multi payment fails if one of attached asset scripts forbids tx") {
    forAll(
      paymentPreconditions(
        dApp(V4, transferPaymentAmount = 1, _),
        accountVerifierGen(V4),
        verifier(V4, Asset),
        additionalAssetScript = Some(
          Gen.oneOf(
            verifier(V4, Asset, result = "throw()"),
            verifier(V4, Asset, result = "false")
          )
        )
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        )(_ should matchPattern {
          case Right(diff: Diff) if diff.transactions.exists(!_._2.applied) =>
        })
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
    forAll(
      paymentPreconditions(
        dApp(V4, transferPaymentAmount = 1, _),
        accountVerifierGen(V4),
        verifier(V4, Asset),
        withEnoughFee = false
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setVerifier, setDApp))),
          TestBlock.create(Seq(ci)),
          features
        ) {
          val expectedFee = (0.005 + 0.004 + 0.004 * (ContractLimits.MaxAttachedPaymentAmount - 1)) * Constants.UnitsInWave
          _ should produce(
            s"Fee in WAVES for InvokeScriptTransaction (${ci.fee} in WAVES) " +
              s"with ${ContractLimits.MaxAttachedPaymentAmount} total scripts invoked " +
              s"does not exceed minimal value of ${expectedFee.toLong} WAVES"
          )
        }
    }
  }

  property("single payment V3 scripts with activated multi payment") {
    val wavesTransfer = 100
    forAll(
      paymentPreconditions(
        dApp(V3, transferPaymentAmount = wavesTransfer, _),
        accountVerifierGen(V3),
        verifier(V3, Asset),
        multiPayment = false
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, invoker, fee) =>
        assertDiffAndState(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        ) {
          case (diff, blockchain) =>
            val assetBalance = issues
              .map(_.id())
              .map(IssuedAsset)
              .map(asset => asset -> blockchain.balance(dAppAcc.toAddress, asset))
              .filter(_._2 != 0)
              .toMap

            diff.portfolios(dAppAcc.toAddress).assets shouldBe assetBalance
            diff.portfolios(dAppAcc.toAddress).balance shouldBe -wavesTransfer
            diff.portfolios(invoker.toAddress).balance shouldBe wavesTransfer - fee
        }
    }
  }

  property("disallowed multi payment") {
    val wavesTransfer = 111
    forAll(
      paymentPreconditions(
        dApp(V3, transferPaymentAmount = wavesTransfer, _),
        accountVerifierGen(V3),
        verifier(V3, Asset)
      )
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features.copy(preActivatedFeatures = features.preActivatedFeatures - BlockchainFeatures.BlockV5.id)
        ) { _ should produce("Multiple payments isn't allowed now") }
    }
  }

  private def paymentPreconditions(
      dApp: KeyPair => Script,
      verifier: Gen[Script],
      commonAssetScript: Script,
      additionalAssetScript: Option[Gen[Script]] = None,
      repeatAdditionalAsset: Boolean = false,
      withEnoughFee: Boolean = true,
      multiPayment: Boolean = true
  ) =
    for {
      master        <- accountGen
      invoker       <- accountGen
      ts            <- timestampGen
      fee           <- if (withEnoughFee) ciFee(ContractLimits.MaxAttachedPaymentAmount + 1) else ciFee(1)
      accountScript <- verifier
      commonIssues <- if (multiPayment)
        Gen.listOfN(
          ContractLimits.MaxAttachedPaymentAmount - 1,
          issueV2TransactionGen(invoker, Gen.const(Some(commonAssetScript)))
        )
      else Gen.const(List())
      specialIssue <- issueV2TransactionGen(invoker, additionalAssetScript.fold(Gen.const(none[Script]))(_.map(Some(_))))
    } yield {
      for {
        genesis     <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2    <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        setVerifier <- SetScriptTransaction.selfSigned(1.toByte, invoker, Some(accountScript), fee, ts + 2)
        setDApp     <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp(invoker)), fee, ts + 2)
        (issues, payments) = if (repeatAdditionalAsset) {
          val issues   = specialIssue :: commonIssues.drop(1)
          val payments = (specialIssue :: issues).map(i => Payment(1, IssuedAsset(i.id())))
          (issues, payments)
        } else {
          val issues   = specialIssue :: commonIssues
          val payments = issues.map(i => Payment(1, IssuedAsset(i.id())))
          (issues, payments)
        }
        ci <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, payments, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setVerifier, setDApp, ci, issues, master, invoker, fee)
    }.explicitGet()

  private def assertScriptVersionError(
      message: (Int, ByteStr) => String,
      dAppVersionGen: Gen[StdLibVersion] = V4,
      verifierVersionGen: Gen[StdLibVersion] = V4,
      assetsScriptVersionGen: Gen[StdLibVersion] = V4
  ): Unit =
    forAll(
      for {
        dAppVersion         <- dAppVersionGen
        verifierVersion     <- verifierVersionGen
        assetsScriptVersion <- assetsScriptVersionGen
        (genesis, setVerifier, setDApp, ci, issues, _, _, _) <- paymentPreconditions(
          dApp(dAppVersion, transferPaymentAmount = 1, _),
          accountVerifierGen(verifierVersion),
          verifier(assetsScriptVersion, Asset)
        )
        oldVersion         = List(dAppVersion, verifierVersion, assetsScriptVersion).filter(_ < V4).head
        maybeFailedAssetId = issues.find(_.script.nonEmpty).get.id()
      } yield (genesis, setVerifier, setDApp, ci, issues, oldVersion, maybeFailedAssetId)
    ) {
      case (genesis, setVerifier, setDApp, ci, issues, oldVersion, maybeFailedAssetId) =>
        assertDiffEi(
          Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
          TestBlock.create(Seq(ci)),
          features
        ) {
          case Right(diff: Diff) =>
            val errMsg = diff.scriptResults(diff.transactions.keys.head).error.get.text
            message(oldVersion.id, maybeFailedAssetId).r.findFirstIn(errMsg) shouldBe defined

          case l @ Left(_) =>
            l should produce(message(oldVersion.id, maybeFailedAssetId))
        }
    }

  private def dApp(version: StdLibVersion, transferPaymentAmount: Int, transferRecipient: KeyPair): Script = {
    val resultSyntax = if (version >= V4) "" else "TransferSet"
    TestCompiler(version).compileContract(s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func default() = $resultSyntax([ScriptTransfer(
         |    Address(base58'${transferRecipient.toAddress}'),
         |    $transferPaymentAmount,
         |    unit
         | )])
         |
       """.stripMargin)
  }

  private def dAppVerifier(version: StdLibVersion, usePaymentsField: Boolean): Script = {
    val paymentsField = if (version >= V4) "payments" else "payment"
    val verifierExpr =
      if (usePaymentsField)
        s"""
          | match tx {
          |   case ist: InvokeScriptTransaction => ist.$paymentsField == ist.$paymentsField
          |   case _ => true
          | }
        """.stripMargin
      else "true"

    TestCompiler(version).compileContract(s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Verifier(tx)
         | func verify() = $verifierExpr
         |
       """.stripMargin)
  }

  private def verifier(version: StdLibVersion, scriptType: ScriptType, result: String = "true"): Script =
    TestCompiler(version).compileExpression(
      s"""
         | {-# STDLIB_VERSION ${version.id}      #-}
         | {-# CONTENT_TYPE   EXPRESSION         #-}
         | {-# SCRIPT_TYPE    ${scriptType.text} #-}
         |
         | $result
         |
       """.stripMargin
    )

  private def accountVerifierGen(version: StdLibVersion) =
    for {
      usePaymentsField <- Gen.oneOf(true, false)
      verifier <- if (version >= V3)
        Gen.oneOf(verifier(version, Account), dAppVerifier(version, usePaymentsField))
      else
        Gen.const(verifier(version, Account))
    } yield verifier

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5
    ).map(_.id -> 0).toMap
  )
}
