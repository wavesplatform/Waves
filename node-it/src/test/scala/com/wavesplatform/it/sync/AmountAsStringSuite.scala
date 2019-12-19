package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.asynchttpclient._
import play.api.libs.json.{JsString, JsSuccess, JsValue, Json}

class AmountAsStringSuite extends BaseTransactionSuite {

  val (headerName, headerValue) = ("Accept", "application/json;number-format=string")

  test("amount as string in assets api") {
    val assetId = sender.issue(firstAddress, "assetName", "description", someAssetAmount, 8, fee = issueFee, waitForTx = true).id
    sender.transfer(firstAddress, secondAddress, transferAmount, minFee, Some(assetId), waitForTx = true)
    val currentHeight = sender.height
    val assetDetails  = sender.getWithCustomHeader(s"/assets/details/$assetId", headerName, headerValue)
    val assetBalance  = sender.getWithCustomHeader(s"/assets/balance/$firstAddress/$assetId", headerName, headerValue)
    val assetsBalance = sender.getWithCustomHeader(s"/assets/balance/$firstAddress", headerName, headerValue)

    sender.waitForHeight(currentHeight + 1)
    val assetDistribution = sender.getWithCustomHeader(s"/assets/$assetId/distribution/$currentHeight/limit/1", headerName, headerValue)

    (Json.parse(assetBalance.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(assetsBalance.getResponseBody) \ "balances" \ 0 \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(assetDistribution.getResponseBody) \ "items" \ 0 \ 1).validate[JsString] shouldBe a[JsSuccess[_]]
//    (Json.parse(assetDetails.getResponseBody) \ "quantity").validate[JsString] shouldBe a[JsSuccess[_]]
  }

  test("amount as string in addresses api") {
    val balance = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress", headerName, headerValue)
    (Json.parse(balance.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
    val balanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress/1", headerName, headerValue)
    (Json.parse(balanceWithConfirmations.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]

    val balanceDetails = sender.getWithCustomHeader(s"/addresses/balance/details/$firstAddress", headerName, headerValue)
    (Json.parse(balanceDetails.getResponseBody) \ "regular").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(balanceDetails.getResponseBody) \ "generating").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(balanceDetails.getResponseBody) \ "available").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(balanceDetails.getResponseBody) \ "effective").validate[JsString] shouldBe a[JsSuccess[_]]

    val effectiveBalance                  = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress", headerName, headerValue)
    val effectiveBalanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress/1", headerName, headerValue)
    (Json.parse(effectiveBalance.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(effectiveBalanceWithConfirmations.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
  }

  test("amount as string in transactions api") {
    val exchanger    = KeyPair("exchanger".getBytes)
    val transferTxId = sender.transfer(firstAddress, exchanger.stringRepr, transferAmount, minFee, waitForTx = true).id
    val exchAssetId = sender
      .broadcastIssue(exchanger, "exchange asset", "", someAssetAmount, 8, fee = issueFee, reissuable = true, script = None, waitForTx = true)
      .id
    val ts = System.currentTimeMillis()
    val buyOrder = Order.buy(
      exchanger,
      exchanger.publicKey,
      AssetPair.createAssetPair("WAVES", exchAssetId).get,
      1000000,
      1000,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    val sellOrder = Order.sell(
      exchanger,
      exchanger.publicKey,
      AssetPair.createAssetPair("WAVES", exchAssetId).get,
      1000000,
      1000,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    val exchangeTxId =
      sender.exchange(exchanger, buyOrder, sellOrder, 1000000, 1000, matcherFee, matcherFee, matcherFee, waitForTx = true).id

    val dataEntries = List(BooleanDataEntry("bool", true), IntegerDataEntry("int", 666), BinaryDataEntry("blob", ByteStr.decodeBase58("boop").get))
    val dataTxId = sender.putData(firstAddress, dataEntries, calcDataFee(dataEntries), waitForTx = true).id

    val exchangeTxInfo = sender.getWithCustomHeader(s"/transactions/info/$exchangeTxId", headerName, headerValue)
    (Json.parse(exchangeTxInfo.getResponseBody) \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "price").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "sellMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "buyMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]

    val transferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$transferTxId", headerName, headerValue)
    (Json.parse(transferTxInfo.getResponseBody) \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(transferTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]

    val dataTxInfo = sender.getWithCustomHeader(s"/transactions/info/$dataTxId", headerName, headerValue)
    (Json.parse(dataTxInfo.getResponseBody) \ "data" \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]
    val addressData = sender.getWithCustomHeader(s"/addresses/data/$firstAddress", headerName, headerValue)
    (Json.parse(addressData.getResponseBody) \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]
  }

  test("amount as string in blocks api") {
    val currentHeight    = sender.height
    val blockLast        = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val blockAt          = sender.getWithCustomHeader(s"/blocks/at/$currentHeight", headerName, headerValue)
    val blockHeadersLast = sender.getWithCustomHeader("/blocks/headers/last", headerName, headerValue)
    val blockHeadersAt   = sender.getWithCustomHeader(s"/blocks/headers/at/$currentHeight", headerName, headerValue)
    val blockBySignature = sender.getWithCustomHeader(s"/blocks/signature/${sender.lastBlock.signature}", headerName, headerValue)

    for (block <- Seq(blockLast, blockAt, blockHeadersLast, blockHeadersAt, blockBySignature)) {
      (Json.parse(block.getResponseBody) \ "reward").validate[JsString] shouldBe a[JsSuccess[_]]
      (Json.parse(block.getResponseBody) \ "desiredReward").validate[JsString] shouldBe a[JsSuccess[_]]
      (Json.parse(block.getResponseBody) \ "totalFee").validate[JsString] shouldBe a[JsSuccess[_]]
    }

    val blockSeq        = sender.getWithCustomHeader(s"/blocks/seq/${currentHeight - 1}/$currentHeight", headerName, headerValue)
    val blockHeadersSeq = sender.getWithCustomHeader(s"/blocks/headers/seq/${currentHeight - 1}/$currentHeight", headerName, headerValue)
    val blockSeqByAddress =
      sender.getWithCustomHeader(s"/blocks/address/${sender.address}/${currentHeight - 1}/$currentHeight", headerName, headerValue)

    for (block <- Seq(blockSeq, blockHeadersSeq, blockSeqByAddress)) {
      (Json.parse(block.getResponseBody) \ 0 \ "reward").validate[JsString] shouldBe a[JsSuccess[_]]
      (Json.parse(block.getResponseBody) \ 0 \ "desiredReward").validate[JsString] shouldBe a[JsSuccess[_]]
      (Json.parse(block.getResponseBody) \ 0 \ "totalFee").validate[JsString] shouldBe a[JsSuccess[_]]
    }
  }

  test("amount as string in rewards api") {
    val rewards         = sender.getWithCustomHeader("/blockchain/rewards", headerName, headerValue)
    val rewardsByHeight = sender.getWithCustomHeader(s"/blockchain/rewards/${sender.height}", headerName, headerValue)
    (Json.parse(rewards.getResponseBody) \ "totalWavesAmount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(rewards.getResponseBody) \ "currentReward").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(rewards.getResponseBody) \ "minIncrement").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(rewardsByHeight.getResponseBody) \ "totalWavesAmount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(rewardsByHeight.getResponseBody) \ "currentReward").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(rewardsByHeight.getResponseBody) \ "minIncrement").validate[JsString] shouldBe a[JsSuccess[_]]

  }

  test("amount as string in consensus api") {
    val generatingBalance = sender.getWithCustomHeader(s"/consensus/generatingbalance/$firstAddress", headerName, headerValue)
    (Json.parse(generatingBalance.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
  }

  test("amount as string in debug api") {
    val portfolio = sender.getWithCustomHeader(s"/debug/portfolios/$firstAddress", headerName, headerValue, withApiKey = true)
    (Json.parse(portfolio.getResponseBody) \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(portfolio.getResponseBody) \ "lease" \ "in").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(portfolio.getResponseBody) \ "lease" \ "out").validate[JsString] shouldBe a[JsSuccess[_]]

    val state = sender.getWithCustomHeader("/debug/state", headerName, headerValue, withApiKey = true)
    (Json.parse(state.getResponseBody) \ s"$firstAddress").validate[JsString] shouldBe a[JsSuccess[_]]

    val balanceHistory = sender.getWithCustomHeader(s"/debug/balances/history/$firstAddress", headerName, headerValue, withApiKey = true)
    (Json.parse(balanceHistory.getResponseBody) \ 0 \ "balance").validate[JsString] shouldBe a[JsSuccess[_]]

    val stateWavesOnHeight = sender.getWithCustomHeader(s"/debug/stateWaves/${sender.height}", headerName, headerValue, withApiKey = true)
    (Json.parse(stateWavesOnHeight.getResponseBody) \ s"$firstAddress").validate[JsString] shouldBe a[JsSuccess[_]]
  }

}
