package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V8}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeConstants
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, TransactionType, TxHelpers, TxVersion}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class OverdraftTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val InvokeFee = FeeConstants(TransactionType.InvokeScript) * FeeValidation.FeeUnit
  private val SetScriptFee = FeeConstants(TransactionType.SetScript) * FeeValidation.FeeUnit
  private val IssueFee  = FeeConstants(TransactionType.Issue) * FeeValidation.FeeUnit

  private val dAppVersions: List[StdLibVersion] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .toList

  private val dAppVersionWithSettings: Seq[(StdLibVersion, FunctionalitySettings)] =
    dAppVersions.map { version =>
      (version, settingsForRide(version).blockchainSettings.functionalitySettings)
    }

  private val allActivatedSettings =
    settingsForRide(DirectiveDictionary[StdLibVersion].all.last).blockchainSettings.functionalitySettings

  property("insufficient fee") {
    dAppVersionWithSettings.foreach {
      case (version, settings) =>
        val (genesis, setDApp, ci, _) = paymentPreconditions(withEnoughFee = false, withPayment = false, emptyResultDApp(version))

        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), settings) { r =>
            r should produce(
            s"Fee for InvokeScriptTransaction (1 in WAVES) does not exceed minimal value of $InvokeFee WAVES"
          )
      }
    }
  }

  property("overdraft") {
    dAppVersionWithSettings.foreach {
      case (version, settings) =>
        val (genesis, setDApp, ci, _) = paymentPreconditions(withEnoughFee = true, withPayment = false, payingDApp(version))

        assertDiffEi(Seq(TestBlock.create(genesis :+ setDApp)), TestBlock.create(Seq(ci)), settings) { r =>
          if (settings.preActivatedFeatures.contains(BlockchainFeatures.BlockV5.id))
            r should produce("AccountBalanceError")
          else
            r.explicitGet()
        }
    }
  }

  property("overdraft with payment V3") {
    dAppVersionWithSettings.filter(_._1 < V8).foreach {
      case (_, settings) =>
        val (genesis, setDApp, ci, issue) = paymentPreconditions(withEnoughFee = true, withPayment = true, payingDApp(V3))

        assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), settings) {
          _ should produce("leads to negative waves balance to (at least) temporary negative state")
        }
    }
  }

  property("overdraft with payment V4") {
    dAppVersions.filter(_ >= V4).foreach { version =>
      val (genesis, setDApp, ci, issue) = paymentPreconditions(withEnoughFee = true, withPayment = true, payingDApp(version))

      assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(ci)), allActivatedSettings) {
        _ should produce("AccountBalanceError")
      }
    }
  }

  property("attach unexisting tokens using multiple payment") {
    dAppVersions.filter(_ > V3).foreach { version =>
      val master  = TxHelpers.signer(0)
      val invoker = TxHelpers.signer(1)

      val genesis = Seq(
        TxHelpers.genesis(master.toAddress),
        TxHelpers.genesis(invoker.toAddress)
      )
      val issue   = TxHelpers.issue(invoker)
      val setDApp = TxHelpers.setScript(master, payingAssetDApp(version, issue.assetId))

      val count    = ContractLimits.MaxAttachedPaymentAmount
      val payments = (1 to count).map(_ => Payment(issue.quantity.value / count + 1, IssuedAsset(issue.id())))
      val invoke   = TxHelpers.invoke(master.toAddress, func = None, invoker = invoker, payments = payments)

      assertDiffEi(Seq(TestBlock.create(genesis ++ List(setDApp, issue))), TestBlock.create(Seq(invoke)), allActivatedSettings) {
        _ should produce("Attempt to transfer unavailable funds: Transaction application leads to negative asset")
      }
    }
  }

  private def paymentPreconditions(
      withEnoughFee: Boolean,
      withPayment: Boolean,
      dApp: Script
  ): (Seq[GenesisTransaction], SetScriptTransaction, InvokeScriptTransaction, IssueTransaction) = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(invoker.toAddress, if (withPayment) IssueFee else 0)
    )
    val setDApp = TxHelpers.setScript(master, dApp, fee = SetScriptFee)
    val issue   = TxHelpers.issue(invoker, fee = IssueFee)

    val invoke = TxHelpers.invoke(
      dApp = master.toAddress,
      func = None,
      invoker = invoker,
      payments = if (withPayment) List(Payment(issue.quantity.value, IssuedAsset(issue.id()))) else Nil,
      fee = if (withEnoughFee) InvokeFee else 1,
      version = TxVersion.V1
    )

    (genesis, setDApp, invoke, issue)
  }

  private def emptyResultDApp(version: StdLibVersion): Script = {
    val body = if (version >= V4) "[]" else "WriteSet([])"
    dApp(body, version)
  }

  private def payingDApp(version: StdLibVersion): Script = {
    val transfer = s"ScriptTransfer(i.caller, $InvokeFee, unit)"
    val body     = if (version >= V4) s"[$transfer]" else s"TransferSet([$transfer])"
    dApp(body, version)
  }

  private def payingAssetDApp(version: StdLibVersion, assetId: ByteStr): Script = {
    val transfer = s"ScriptTransfer(i.caller, $InvokeFee, base58'${assetId.toString}')"
    val body     = if (version >= V4) s"[$transfer]" else s"TransferSet([$transfer])"
    dApp(body, version)
  }

  private def dApp(body: String, version: StdLibVersion): Script = TestCompiler(version).compileContract(s"""
    | {-# STDLIB_VERSION $version #-}
    | {-# CONTENT_TYPE   DAPP     #-}
    | {-# SCRIPT_TYPE    ACCOUNT  #-}
    |
    | @Callable(i)
    | func default() = $body
    |
    |""".stripMargin)
}
