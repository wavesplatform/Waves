package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.{Node, NodeConfigs}
import org.scalatest.prop.TableDrivenPropertyChecks
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{calcMassTransferFee, setScriptFee, transferAmount}
import com.wavesplatform.it.util._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.ExchangeTransactionV1
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer

class TransferNFTSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.raw("""waves {
                            |  miner.quorum = 0
                            |  blockchain.custom.functionality.pre-activated-features.13 = 0
                            |}""".stripMargin))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
  val assetName        = "NFTAsset"
  val assetDescription = "my asset description"

  test("NFT should correctly be transferred via transfer transaction"){
    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.transfer(firstAddress, secondAddress, 1, minFee, Some(nftAsset), waitForTx = true)

    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain atLeastOneElementOf List(nftAsset)
    sender.assetBalance(secondAddress, nftAsset).balance shouldBe 1 // passed, but should be failed because /assets/balance returns only non-nft assets
    sender.nftAssetsBalance(secondAddress, 10).map(info => info.assetId) shouldBe List(nftAsset)
  }

  test("NFT should correctly be transferred via invoke script transaction") {
    val scriptText =
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
        |@Callable(i)
         |func nftTransfer() = {
         |    let pmt = i.payment.extract()
         |    TransferSet([
         |            ScriptTransfer(this, pmt.amount, pmt.assetId)
         |        ])
         |}
         |
        | @Verifier(t)
         | func verify() = {
         |  true
         | }
         |
        |
        """.stripMargin
    val script      = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(secondAddress, Some(script), setScriptFee, waitForTx = true)

    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.invokeScript(
      firstAddress,
      secondAddress,
      Some("nftTransfer"),
      payment = Seq(
        InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))
      ),
      fee = 1300000,
      waitForTx = true
    )

    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain atLeastOneElementOf List(nftAsset)
    sender.assetBalance(secondAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(secondAddress, 10).map(info => info.assetId) should contain oneElementOf List(nftAsset)
  }

  test("NFT should correctly be transferred via mass transfer transaction") {
    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    val transfers = List(Transfer(thirdAddress, 1))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)

    sender.massTransfer(firstAddress, transfers, massTransferTransactionFee, Some(nftAsset), waitForTx = true)

    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain atLeastOneElementOf List(nftAsset)
    sender.assetBalance(secondAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(secondAddress, 10).map(info => info.assetId) should contain oneElementOf List(nftAsset)
  }
}
