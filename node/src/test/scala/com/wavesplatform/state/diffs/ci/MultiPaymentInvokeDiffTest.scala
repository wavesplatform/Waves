package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.Diff
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}

class MultiPaymentInvokeDiffTest extends PropSpec with WithState {
  private val oldVersions = Seq(V1, V2, V3)

  property("multi payment with verifier and transfer set") {
    val wavesTransfer = 111
    paymentPreconditions(
      dApp(V4, transferPaymentAmount = wavesTransfer, _),
      accountVerifiers(V4),
      verifier(V4, Asset)
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, invoker, fee) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id())
          .map(IssuedAsset(_))
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
    paymentPreconditions(
      dApp(V4, transferPaymentAmount = 1, _),
      accountVerifiers(V4),
      verifier(V4, Asset),
      repeatAdditionalAsset = true
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, _, _) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id())
          .map(IssuedAsset(_))
          .map(asset => asset -> blockchain.balance(dAppAcc.toAddress, asset))
          .filter(_._2 != 0)
          .toMap

        diff.portfolios(dAppAcc.toAddress).assets shouldBe assetBalance
      }
    }
  }

  property("multi payment fails if one of attached asset scripts forbids tx") {
    paymentPreconditions(
      dApp(V4, transferPaymentAmount = 1, _),
      accountVerifiers(V4),
      verifier(V4, Asset),
      additionalAssetScripts = Some(
        Seq(
          verifier(V4, Asset, result = "throw()"),
          verifier(V4, Asset, result = "false")
        )
      )
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      )(_ should matchPattern {
        case Right(diff: Diff) if diff.transactions.exists(_.status != Status.Succeeded) =>
      })
    }
  }

  property("multi payment fails if any script has version lower V4") {
    assertScriptVersionError(
      (v, _) => s"DApp version $v < 4 doesn't support multiple payment attachment",
      dAppVersions = Seq(V3)
    )
    assertScriptVersionError(
      (v, _) => s"Invoker script version $v < 4 doesn't support multiple payment attachment",
      verifierVersions = oldVersions
    )
    assertScriptVersionError(
      (v, id) => s"Attached asset script id=$id version $v < 4 doesn't support multiple payment attachment",
      assetsScriptVersions = oldVersions
    )
  }

  property("fee proportionality") {
    paymentPreconditions(
      dApp(V4, transferPaymentAmount = 1, _),
      accountVerifiers(V4),
      verifier(V4, Asset),
      withEnoughFee = false
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setVerifier, setDApp))),
        TestBlock.create(Seq(ci)),
        features
      ) {
        val expectedFee = (0.005 + 0.004 + 0.004 * (ContractLimits.MaxAttachedPaymentAmount - 1)) * Constants.UnitsInWave
        _ should produceRejectOrFailedDiff(
          s"Fee in WAVES for InvokeScriptTransaction (${ci.fee} in WAVES) " +
            s"with ${ContractLimits.MaxAttachedPaymentAmount} total scripts invoked " +
            s"does not exceed minimal value of ${expectedFee.toLong} WAVES"
        )
      }
    }
  }

  property("single payment V3 scripts with activated multi payment") {
    val wavesTransfer = 100
    paymentPreconditions(
      dApp(V3, transferPaymentAmount = wavesTransfer, _),
      accountVerifiers(V3),
      verifier(V3, Asset),
      multiPayment = false
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, dAppAcc, invoker, fee) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id())
          .map(IssuedAsset(_))
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
    paymentPreconditions(
      dApp(V3, transferPaymentAmount = wavesTransfer, _),
      accountVerifiers(V3),
      verifier(V3, Asset)
    ).foreach { case (genesis, setVerifier, setDApp, ci, issues, _, _, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features.copy(preActivatedFeatures = features.preActivatedFeatures - BlockchainFeatures.BlockV5.id)
      ) { _ should produce("Multiple payments isn't allowed now") }
    }
  }

  private def paymentPreconditions(
      dApp: KeyPair => Script,
      verifiers: Seq[Script],
      commonAssetScript: Script,
      additionalAssetScripts: Option[Seq[Script]] = None,
      repeatAdditionalAsset: Boolean = false,
      withEnoughFee: Boolean = true,
      multiPayment: Boolean = true
  ): Seq[
    (Seq[GenesisTransaction], SetScriptTransaction, SetScriptTransaction, InvokeScriptTransaction, List[IssueTransaction], KeyPair, KeyPair, Long)
  ] = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val fee = if (withEnoughFee) TxHelpers.ciFee(ContractLimits.MaxAttachedPaymentAmount + 1) else TxHelpers.ciFee(1)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress)
    )
    val commonIssues = if (multiPayment) {
      (1 until ContractLimits.MaxAttachedPaymentAmount)
        .map(idx => TxHelpers.issue(invoker, name = s"asset$idx", script = Some(commonAssetScript)))
        .toList
    } else {
      List.empty
    }

    for {
      accountScript <- verifiers
      additionalAssetScript <-
        if (additionalAssetScripts.exists(_.nonEmpty)) {
          additionalAssetScripts.toSeq.flatten.map(Some(_))
        } else {
          Seq(None)
        }
    } yield {
      val setVerifier  = TxHelpers.setScript(invoker, accountScript)
      val setDApp      = TxHelpers.setScript(master, dApp(invoker))
      val specialIssue = TxHelpers.issue(invoker, name = "special", script = additionalAssetScript)

      val (issues, payments) = if (repeatAdditionalAsset) {
        val issues   = specialIssue :: commonIssues.drop(1)
        val payments = (specialIssue :: issues).map(i => Payment(1, IssuedAsset(i.id())))
        (issues, payments)
      } else {
        val issues   = specialIssue :: commonIssues
        val payments = issues.map(i => Payment(1, IssuedAsset(i.id())))
        (issues, payments)
      }

      val invoke = TxHelpers.invoke(master.toAddress, func = None, payments = payments, invoker = invoker, fee = fee, version = TxVersion.V1)

      (genesis, setVerifier, setDApp, invoke, issues, master, invoker, fee)
    }
  }

  private def assertScriptVersionError(
      message: (Int, ByteStr) => String,
      dAppVersions: Seq[StdLibVersion] = Seq(V4),
      verifierVersions: Seq[StdLibVersion] = Seq(V4),
      assetsScriptVersions: Seq[StdLibVersion] = Seq(V4)
  ): Unit = {
    for {
      dAppVersion         <- dAppVersions
      verifierVersion     <- verifierVersions
      assetsScriptVersion <- assetsScriptVersions
      (genesis, setVerifier, setDApp, ci, issues, _, _, _) <- paymentPreconditions(
        dApp(dAppVersion, transferPaymentAmount = 1, _),
        accountVerifiers(verifierVersion),
        verifier(assetsScriptVersion, Asset)
      )
    } yield {
      val oldVersion         = List(dAppVersion, verifierVersion, assetsScriptVersion).filter(_ < V4).head
      val maybeFailedAssetId = issues.find(_.script.nonEmpty).get.id()

      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) {
        case Right(diff: Diff) =>
          val errMsg = diff.scriptResults(diff.transactions.head.transaction.id()).error.get.text
          message(oldVersion.id, maybeFailedAssetId).r.findFirstIn(errMsg) shouldBe defined

        case l @ Left(_) =>
          l should produce(message(oldVersion.id, maybeFailedAssetId))
      }
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

  private def accountVerifiers(version: StdLibVersion): Seq[Script] =
    if (version >= V3)
      Seq(verifier(version, Account), dAppVerifier(version, usePaymentsField = true), dAppVerifier(version, usePaymentsField = false))
    else
      Seq(verifier(version, Account))

  private val features = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
    Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5
    ).map(_.id -> 0).toMap
  )
}
