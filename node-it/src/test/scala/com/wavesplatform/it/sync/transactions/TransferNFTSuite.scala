package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.sync.{calcMassTransferFee, setScriptFee, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer

class TransferNFTSuite extends BaseTransactionSuite with NTPTime {
  val assetName        = "NFTAsset"
  val assetDescription = "my asset description"

  val caller = firstAddress
  val dApp = secondAddress
  val receiver = thirdAddress

  test("NFT should be correctly transferred via transfer transaction") {
    val nftAsset = sender.issue(caller, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.transfer(caller, dApp, 1, minFee, Some(nftAsset), waitForTx = true)

    sender.assetBalance(caller, nftAsset).balance shouldBe 0
    sender.nftList(caller, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 1
    sender.nftList(dApp, 10).map(info => info.assetId) should contain(nftAsset)
  }

  test("NFT should be correctly transferred via invoke script transaction") {
    val nftAsset = sender.issue(caller, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    val scriptText =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable(i)
         |func nftTransferToDapp() = {
         |    let pmt = i.payments[0];
         |    [ ScriptTransfer(this, pmt.amount, pmt.assetId) ]
         |}
         |
         |@Callable(i)
         |func nftPaymentTransferToThirdAddress(address: String) = {
         |    let thirdAddress = Address(fromBase58String(address))
         |    let pmt = i.payments[0];
         |    [ ScriptTransfer(thirdAddress, pmt.amount, pmt.assetId) ]
         |}
         |
         |@Callable(i)
         |func transferAsPayment() = []
         |
         |@Callable(i)
         |func nftTransferToSelf() = {
         |    let pmt = i.payments[0];
         |    [ ScriptTransfer(i.caller, pmt.amount, pmt.assetId) ]
         |}
         |
         |@Callable(i)
         |func transferFromDappToAddress(address: String) = {
         |    let recipient = Address(fromBase58String(address))
         |    [ ScriptTransfer(recipient, 1, base58'$nftAsset') ]
         |}
         |
         |@Verifier(t)
         |func verify() = true
         |""".stripMargin
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)
    def invokeTransfer(
        caller: String,
        functionName: String,
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty
    ): Transaction = {
      sender.invokeScript(caller, dApp, Some(functionName), payment = payment, args = args, fee = 1300000, waitForTx = true)._1
    }
    val nftPayment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset))))

    val tx = invokeTransfer(caller, "nftTransferToDapp", payment = nftPayment)
    sender.debugStateChanges(tx.id).stateChanges.get.errorMessage.get.text should include("DApp self-transfer is forbidden")

    sender.transfer(caller, dApp, 1, assetId = Some(nftAsset), waitForTx = true)

    invokeTransfer(caller, "transferFromDappToAddress", args = List(Terms.CONST_STRING(receiver).explicitGet()))
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftList(dApp, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(receiver, nftAsset).balance shouldBe 1
    sender.nftList(receiver, 10).map(info => info.assetId) should contain(nftAsset)

    invokeTransfer(receiver, "nftTransferToSelf", payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))))
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftList(dApp, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(receiver, nftAsset).balance shouldBe 1
    sender.nftList(receiver, 10).map(info => info.assetId) should contain(nftAsset)

    invokeTransfer(
      receiver,
      "nftPaymentTransferToThirdAddress",
      args = List(Terms.CONST_STRING(caller).explicitGet()),
      payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset))))
    )
    sender.assetBalance(receiver, nftAsset).balance shouldBe 0
    sender.nftList(receiver, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 0
    sender.nftList(dApp, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(caller, nftAsset).balance shouldBe 1
    sender.nftList(caller, 10).map(info => info.assetId) should contain(nftAsset)

    invokeTransfer(caller, "transferAsPayment", payment = Seq(InvokeScriptTransaction.Payment(1, Asset.fromString(Some(nftAsset)))))
    sender.assetBalance(caller, nftAsset).balance shouldBe 0
    sender.nftList(caller, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(dApp, nftAsset).balance shouldBe 1
    sender.nftList(dApp, 10).map(info => info.assetId) should contain(nftAsset)
  }

  test("NFT should be correctly transferred via mass transfer transaction") {
    val nftAsset = sender.issue(caller, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true).id
    sender.massTransfer(caller, List(Transfer(receiver, 1)), calcMassTransferFee(1), assetId = Some(nftAsset), waitForTx = true)

    sender.assetBalance(caller, nftAsset).balance shouldBe 0
    sender.nftList(caller, 10).map(info => info.assetId) shouldNot contain(nftAsset)
    sender.assetBalance(receiver, nftAsset).balance shouldBe 1
    sender.nftList(receiver, 10).map(info => info.assetId) should contain(nftAsset)
  }

  test("NFT should correctly be transferred via exchange transaction") {
    val buyer     = KeyPair("buyer".getBytes("UTF-8"))
    val seller    = KeyPair("seller".getBytes("UTF-8"))
    val matcher   = KeyPair("matcher".getBytes("UTF-8"))
    val transfers = List(Transfer(buyer.toAddress.toString, 10.waves), Transfer(seller.toAddress.toString, 10.waves), Transfer(matcher.toAddress.toString, 10.waves))
    sender.massTransfer(caller, transfers, calcMassTransferFee(transfers.size), waitForTx = true)

    val nftAsset =
      sender.broadcastIssue(seller, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true, script = None).id
    val pair = AssetPair.createAssetPair(nftAsset, "WAVES")
    val ts   = ntpTime.correctedTime()
    val buy = Order.buy(
      Order.V2,
      sender = buyer,
      matcher = matcher.publicKey,
      pair = pair.get,
      amount = 1,
      price = 1.waves,
      timestamp = ts,
      expiration = ts + Order.MaxLiveTime,
      matcherFee = matcherFee
    )
    val sell = Order.sell(
      Order.V2,
      sender = seller,
      matcher = matcher.publicKey,
      pair = pair.get,
      amount = 1,
      price = 1.waves,
      timestamp = ts,
      expiration = ts + Order.MaxLiveTime,
      matcherFee = matcherFee
    )

    val tx = ExchangeTransaction
      .signed(
        2.toByte,
        matcher = matcher.privateKey,
        order1 = buy,
        order2 = sell,
        amount = 1,
        price = 1.waves,
        buyMatcherFee = matcherFee,
        sellMatcherFee = matcherFee,
        fee = matcherFee,
        timestamp = ts
      )
      .explicitGet()
      .json()

    sender.signedBroadcast(tx, waitForTx = true)
    sender.nftList(buyer.toAddress.toString, 10).map(info => info.assetId) should contain oneElementOf List(nftAsset)
    sender.nftList(seller.toAddress.toString, 10).map(info => info.assetId) shouldNot contain atLeastOneElementOf List(nftAsset)
    sender.assetBalance(buyer.toAddress.toString, nftAsset).balance shouldBe 1
    sender.assetBalance(seller.toAddress.toString, nftAsset).balance shouldBe 0

  }

}
