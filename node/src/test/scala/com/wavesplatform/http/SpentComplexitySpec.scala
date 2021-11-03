package com.wavesplatform.http

import com.wavesplatform.api.http.{ApiMarshallers, TransactionsApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, GenesisTransaction}
import com.wavesplatform.{BlockGen, TestWallet}
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import play.api.libs.json.JsObject

class SpentComplexitySpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with BlockGen
    with OptionValues
    with TestWallet
    with WithDomain
    with ApiMarshallers {
  private val contract = TestCompiler(V5)
    .compileContract("""{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Verifier(tx)
      |func verify() = {
      |  let i1 = if (sigVerify(tx.bodyBytes, tx.proofs[1], tx.senderPublicKey)) then 1 else 0
      |  let i2 = if (sigVerify(tx.bodyBytes, tx.proofs[2], tx.senderPublicKey)) then 1 else 0
      |  let i3 = if (sigVerify(tx.bodyBytes, tx.proofs[3], tx.senderPublicKey)) then 1 else 0
      |  i1 + i2 + i3 < 10
      |}
      |
      |@Callable(i)
      |func default() = {
      |  [StringEntry("a", "b")]
      |}
      |""".stripMargin)

  private val assetScript = TestCompiler(V5)
    .compileAsset("""{-# STDLIB_VERSION 5 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |
      |let i1 = if (sigVerify(tx.bodyBytes, tx.bodyBytes, tx.senderPublicKey)) then 1 else 0
      |let i2 = if (sigVerify(tx.bodyBytes, tx.bodyBytes, tx.senderPublicKey)) then 1 else 0
      |
      |i1 + i2 < 10
      |""".stripMargin)

  private val settings =
    domainSettingsWithPreactivatedFeatures(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAssets,
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.SynchronousCalls
    )

  private val sender             = testWallet.generateNewAccount().get
  private val genesisTransaction = GenesisTransaction.create(sender.toAddress, 10_000_00000000L, ntpTime.getTimestamp()).explicitGet()

  private def route(d: Domain) =
    seal(
      TransactionsApiRoute(restAPISettings, d.transactionsApi, testWallet, d.blockchain, () => 0, DummyTransactionPublisher.accepting, ntpTime).route
    )

  "Invocation" - {
    "does not count verifier complexity when InvokeScript is sent from smart account" in withDomain(settings) { d =>
      val invokeTx = InvokeScriptTransaction
        .selfSigned(2.toByte, sender, sender.toAddress, None, Seq.empty, 90_0000L, Asset.Waves, ntpTime.getTimestamp())
        .explicitGet()

      d.appendBlock(
        genesisTransaction,
        SetScriptTransaction.selfSigned(2.toByte, sender, Some(contract), 100_0000L, ntpTime.getTimestamp()).explicitGet(),
        invokeTx
      )

      Get(s"/transactions/info/${invokeTx.id()}") ~> route(d) ~> check {
        (responseAs[JsObject] \ "spentComplexity").as[Long] shouldBe 2
      }
    }

    "counts asset script complexity when smart asset payment is attached" in withDomain(settings) { d =>
      val issue = IssueTransaction
        .selfSigned(2.toByte, sender, "TEST", "", 1000_00L, 2.toByte, false, Some(assetScript), 1_00000000L, ntpTime.getTimestamp())
        .explicitGet()

      val recipient = testWallet.generateNewAccount().get
      val transferAsset = TransferTransaction
        .selfSigned(2.toByte, sender, recipient.toAddress, issue.asset, 50_00L, Waves, 40_0000L, ByteStr.empty, ntpTime.getTimestamp())
        .explicitGet()

      val invokeTx = InvokeScriptTransaction
        .selfSigned(2.toByte, recipient, sender.toAddress, None, Seq(InvokeScriptTransaction.Payment(50_00L, issue.asset)), 90_0000L, Asset.Waves, ntpTime.getTimestamp())
        .explicitGet()

      d.appendBlock(
        genesisTransaction,
        GenesisTransaction.create(recipient.toAddress, 10_00000000L, ntpTime.getTimestamp()).explicitGet(),
        issue,
        transferAsset,
        SetScriptTransaction.selfSigned(2.toByte, sender, Some(contract), 100_0000L, ntpTime.getTimestamp()).explicitGet(),
        invokeTx
      )

      Get(s"/transactions/info/${invokeTx.id()}") ~> route(d) ~> check {
        (responseAs[JsObject] \ "spentComplexity").as[Long] shouldBe 2
      }
    }
  }

  "Does not count smart asset complexity for transfer" in withDomain(settings) { d =>
    val issue = IssueTransaction
      .selfSigned(2.toByte, sender, "TEST", "", 1000_00L, 2.toByte, false, Some(assetScript), 1_00000000L, ntpTime.getTimestamp())
      .explicitGet()

    val recipient = testWallet.generateNewAccount().get
    val transferAsset = TransferTransaction
      .selfSigned(2.toByte, sender, recipient.toAddress, issue.asset, 50_00L, Waves, 40_0000L, ByteStr.empty, ntpTime.getTimestamp())
      .explicitGet()

    val returnFrom = TransferTransaction
      .selfSigned(2.toByte, recipient, sender.toAddress, issue.asset, 49_00L, Waves, 40_0000L, ByteStr.empty, ntpTime.getTimestamp())
      .explicitGet()

    d.appendBlock(
      genesisTransaction,
      GenesisTransaction.create(recipient.toAddress, 10_00000000L, ntpTime.getTimestamp()).explicitGet(),
      issue,
      transferAsset,
      returnFrom
    )

    val currentRoute = route(d)

    Get(routePath(s"/info/${transferAsset.id()}")) ~> currentRoute ~> check {
      (responseAs[JsObject] \ "spentComplexity").as[Long] shouldBe 0
    }
  }
}
