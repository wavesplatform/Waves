package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{calcMassTransferFee, setScriptFee}
import com.wavesplatform.it.util._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV2, Order}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer

class TransferNFTSuite extends BaseTransactionSuite with NTPTime {
  val assetName        = "NFTAsset"
  val assetDescription = "my asset description"

  test("NFT should be correctly transferred via transfer transaction"){
    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.transfer(firstAddress, secondAddress, 1, minFee, Some(nftAsset), waitForTx = true)

    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(secondAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(secondAddress, 10).map(info => info.assetId) should contain (nftAsset)
  }

  test("NFT should be correctly transferred via invoke script transaction") {
    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    val dApp = secondAddress
    val scriptText =
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func nftTransferToDapp() = {
         |    let pmt = i.payment.extract()
         |    TransferSet([
         |            ScriptTransfer(this, pmt.amount, pmt.assetId)
         |        ])
         |}
         |
         |@Callable(i)
         |func nftPaymentTransferToThirdAddress(address: String) = {
         |    let thirdAddress = Address(fromBase58String(address))
         |    let pmt = i.payment.extract()
         |    TransferSet([
         |            ScriptTransfer(thirdAddress, pmt.amount, pmt.assetId)
         |        ])
         |}
         |
         |@Callable(i)
         |func transferAsPayment() = {
         |    TransferSet([])
         |    }
         |
         |@Callable(i)
         |func nftTransferToSelf() = {
         |    let pmt = i.payment.extract()
         |    TransferSet([
         |            ScriptTransfer(i.caller, pmt.amount, pmt.assetId)
         |        ])
         |}
         |@Callable(i)
         |func transferFromDappToAddress(address: String) = {
         |    let recipient = Address(fromBase58String(address))
         |    TransferSet([
         |            ScriptTransfer(recipient, 1, base58'$nftAsset')
         |        ])
         |}
         |@Verifier(t)
         |func verify() = {
         | true
         |}
        """.stripMargin
    val script      = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)
    def invokeTransfer(caller: String, functionName: String, args: List[Terms.EXPR] = List.empty, payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty): Transaction = {
    sender.invokeScript(
      caller,
      dApp,
      Some(functionName),
      payment = payment,
      args = args,
      fee = 1300000,
      waitForTx = true)._1
    }
    val nftPayment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset))))

    invokeTransfer(firstAddress,"nftTransferToDapp",payment = nftPayment)
    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(dApp, 10).map(info => info.assetId) should contain (nftAsset)

    invokeTransfer(firstAddress, "transferFromDappToAddress",args = List(Terms.CONST_STRING(thirdAddress).explicitGet()))
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(dApp, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(thirdAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(thirdAddress, 10).map(info => info.assetId) should contain (nftAsset)

    invokeTransfer(thirdAddress, "nftTransferToSelf",payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))))
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(dApp, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(thirdAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(thirdAddress, 10).map(info => info.assetId) should contain (nftAsset)

    invokeTransfer(thirdAddress, "nftPaymentTransferToThirdAddress",
      args = List(Terms.CONST_STRING(firstAddress).explicitGet()),
      payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))))
    sender.assetBalance(thirdAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(thirdAddress, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(dApp, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) should contain (nftAsset)

    invokeTransfer(firstAddress, "transferAsPayment",payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))))
    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(dApp, 10).map(info => info.assetId) should contain (nftAsset)
  }

  test("NFT should be correctly transferred via mass transfer transaction") {
    val nftAsset = sender.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.massTransfer(firstAddress, List(Transfer(thirdAddress, 1)), calcMassTransferFee(1), Some(nftAsset), waitForTx = true)

    sender.assetBalance(firstAddress, nftAsset).balance shouldBe 0
    sender.nftAssetsBalance(firstAddress, 10).map(info => info.assetId) shouldNot contain (nftAsset)
    sender.assetBalance(thirdAddress, nftAsset).balance shouldBe 1
    sender.nftAssetsBalance(thirdAddress, 10).map(info => info.assetId) should contain (nftAsset)
  }

  test("NFT should correctly be transferred via exchange transaction") {
    val buyer = KeyPair("buyer".getBytes("UTF-8"))
    val seller  = KeyPair("seller".getBytes("UTF-8"))
    val matcher = KeyPair("matcher".getBytes("UTF-8"))
    val transfers = List(Transfer(buyer.stringRepr, 10.waves), Transfer(seller.stringRepr, 10.waves), Transfer(matcher.stringRepr, 10.waves))
    sender.massTransfer(firstAddress, transfers, calcMassTransferFee(transfers.size), waitForTx = true)

    val nftAsset = sender.broadcastIssue(seller, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true, script = None).id
    val pair = AssetPair.createAssetPair(nftAsset,"WAVES")
    val ts = ntpTime.correctedTime()
    val buy = Order.buy(buyer, matcher, pair.get, 1, 1.waves, ts, ts + Order.MaxLiveTime, matcherFee)
    val sell = Order.sell(seller, matcher, pair.get, 1, 1.waves, ts, ts + Order.MaxLiveTime, matcherFee)

    val tx = ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        amount = 1,
        price = 1.waves,
        buyMatcherFee = matcherFee,
        sellMatcherFee = matcherFee,
        fee = matcherFee,
        timestamp = ts
      ).explicitGet().json()

    sender.signedBroadcast(tx, waitForTx = true)
    sender.nftAssetsBalance(buyer.stringRepr, 10).map(info => info.assetId) should contain oneElementOf List(nftAsset)
    sender.nftAssetsBalance(seller.stringRepr, 10).map(info => info.assetId) shouldNot contain atLeastOneElementOf List(nftAsset)
    sender.assetBalance(buyer.stringRepr, nftAsset).balance shouldBe 1
    sender.assetBalance(seller.stringRepr, nftAsset).balance shouldBe 0

  }

}
