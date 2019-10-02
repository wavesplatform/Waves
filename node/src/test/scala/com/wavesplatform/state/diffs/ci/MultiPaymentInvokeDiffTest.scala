package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
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
  private def dApp(version: StdLibVersion): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func default() = WriteSet([])
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

  private def verifier(version: StdLibVersion, scriptType: ScriptType): Script = {
    val script =
      s"""
         | {-# STDLIB_VERSION ${version.id}      #-}
         | {-# CONTENT_TYPE   EXPRESSION         #-}
         | {-# SCRIPT_TYPE    ${scriptType.text} #-}
         |
         | true
         |
       """.stripMargin

    val expr = Parser.parseExpr(script).get.value
    val compiled = compileExpr(expr, version, scriptType)
    ExprScript(version, compiled).explicitGet()
  }

  private def accountVerifierGen(version: StdLibVersion) =
    Gen.oneOf(verifier(version, Account), dAppVerifier(version))

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Seq(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.MultiPaymentInvokeScript,
    ).map(_.id -> 0).toMap
  )

  private def multiPaymentPreconditions(
    dApp:         Script,
    verifier:     Gen[Script],
    assetScript:  Script
  ): Gen[(List[GenesisTransaction], SetScriptTransaction, SetScriptTransaction, InvokeScriptTransaction, List[IssueTransaction], KeyPair)] =
    for {
      master        <- accountGen
      invoker       <- accountGen
      ts            <- timestampGen
      fee           <- ciFee(10)
      accountScript <- verifier
      issues        <- Gen.listOfN(
        ContractLimits.MaxAttachedPaymentAmount,
        smartIssueTransactionGen(invoker, Gen.const(Some(assetScript)))
      )
    } yield {
      for {
        genesis     <- GenesisTransaction.create(master, ENOUGH_AMT, ts)
        genesis2    <- GenesisTransaction.create(invoker, ENOUGH_AMT, ts)
        setVerifier <- SetScriptTransaction.selfSigned(invoker, Some(accountScript), fee, ts + 2)
        setDApp     <- SetScriptTransaction.selfSigned(master, Some(dApp), fee, ts + 2)
        payments = issues.map(issue => Payment(1, IssuedAsset(issue.id.value)))
        ci <- InvokeScriptTransaction.selfSigned(invoker, master, None, payments, fee, Waves, ts + 3)
      } yield (List(genesis, genesis2), setVerifier, setDApp, ci, issues, master)
    }.explicitGet()

  private def assertScriptError(
    dAppVersion:         StdLibVersion,
    verifierVersion:     StdLibVersion,
    assetsScriptVersion: StdLibVersion,
    message: String
  ): Unit =
    forAll(multiPaymentPreconditions(
      dApp(dAppVersion),
      accountVerifierGen(verifierVersion),
      verifier(assetsScriptVersion, Asset)
    )) { case (genesis, setVerifier, setDApp, ci, issues, _) =>
      assertDiffEi(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      )(_ should produce("Script doesn't support multiple payments"))
    }

  property("multi payment fails if any script has version lower V4") {
    assertScriptError(dAppVersion = V3, V4, V4, "DApp version V3 < V4 doesn't support multiple payment attachment")
    assertScriptError(V4, verifierVersion = V3, V4, "Invoker verifier version V3 < V4 doesn't support multiple payment attachment")
    assertScriptError(V4, V4, assetsScriptVersion = V3, "Attached asset script version V3 < V4 doesn't support multiple payment attachment")
  }

  property("multi payment with verifier and scripted assets transfer") {
    forAll(
      multiPaymentPreconditions(
        dApp(V4),
        accountVerifierGen(V4),
        verifier(V4, Asset)
      )
    ) { case (genesis, setDApp, setVerifier, ci, issues, recipient) =>
      assertDiffAndState(
        Seq(TestBlock.create(genesis ++ issues ++ Seq(setDApp, setVerifier))),
        TestBlock.create(Seq(ci)),
        features
      ) { case (diff, blockchain) =>
        val assetBalance = issues
          .map(_.id.value)
          .map(IssuedAsset)
          .map(asset => asset -> blockchain.balance(recipient, asset))
          .toMap

        val assetsDiff = diff.portfolios(recipient).assets

        assetsDiff shouldBe assetBalance
      }
    }
  }
}
