package com.wavesplatform.it.sync

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry}
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.common.utils.EitherExt2
import org.asynchttpclient._
import play.api.libs.json.{JsString, JsSuccess, JsValue, Json}

class AmountAsStringSuite extends BaseTransactionSuite {

  val (headerName, headerValue) = ("Accept", "application/json;large-significand-format=string")

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
    (Json.parse(assetDetails.getResponseBody) \ "quantity").validate[JsString] shouldBe a[JsSuccess[_]]
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
    val transferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$transferTxId", headerName, headerValue)
    (Json.parse(transferTxInfo.getResponseBody) \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(transferTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]

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
      sender.exchange(exchanger, buyOrder, sellOrder, 1000000, 1000, matcherFee, matcherFee, matcherFee).id
    val utxExchangeTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$exchangeTxId", headerName, headerValue)
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "price").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "sellMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "buyMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
//    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "order1" \ "matcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
//    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "order2" \ "matcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]
    val utx = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx.getResponseBody) \ 0 \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utx.getResponseBody) \ 0 \ "price").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utx.getResponseBody) \ 0 \ "sellMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utx.getResponseBody) \ 0 \ "buyMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
//    (Json.parse(utx.getResponseBody) \ 0 \ "matcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utx.getResponseBody) \ 0 \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]
    sender.waitForTransaction(exchangeTxId)
    val exchangeTxInfo = sender.getWithCustomHeader(s"/transactions/info/$exchangeTxId", headerName, headerValue)
    (Json.parse(exchangeTxInfo.getResponseBody) \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "price").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "sellMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "buyMatcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
//    (Json.parse(exchangeTxInfo.getResponseBody) \ "matcherFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(exchangeTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]

    val dataEntries = List(IntegerDataEntry("int", 666), BinaryDataEntry("blob", ByteStr.decodeBase58("boop").get))
    val dataTxId = sender.putData(firstAddress, dataEntries, calcDataFee(dataEntries)).id
    val utxDataTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$dataTxId", headerName, headerValue)
    (Json.parse(utxDataTxInfo.getResponseBody) \ "data" \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]
    val utx2 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx2.getResponseBody) \ 0 \ "data" \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]
    sender.waitForTransaction(dataTxId)
    val dataTxInfo = sender.getWithCustomHeader(s"/transactions/info/$dataTxId", headerName, headerValue)
    (Json.parse(dataTxInfo.getResponseBody) \ "data" \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]
    val addressData = sender.getWithCustomHeader(s"/addresses/data/$firstAddress", headerName, headerValue)
    (Json.parse(addressData.getResponseBody) \ 0 \ "value").validate[JsString] shouldBe a[JsSuccess[_]]

    val sponsoredAssetId = sender.issue(secondAddress, "sponsor", "", someAssetAmount, 8, waitForTx = true).id
    val sponsorshipTxId = sender.sponsorAsset(secondAddress, sponsoredAssetId, 10000).id
    val utxSponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$sponsorshipTxId", headerName, headerValue)
    (Json.parse(utxSponsorshipTxInfo.getResponseBody) \ "minSponsoredAssetFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utxSponsorshipTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]
    val utx3 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx3.getResponseBody) \ 0 \ "minSponsoredAssetFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(utx3.getResponseBody) \ 0 \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]
    sender.waitForTransaction(sponsorshipTxId)
    val sponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/info/$sponsorshipTxId", headerName, headerValue)
    (Json.parse(sponsorshipTxInfo.getResponseBody) \ "minSponsoredAssetFee").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(sponsorshipTxInfo.getResponseBody) \ "fee").validate[JsString] shouldBe a[JsSuccess[_]]
    val sponsorInfoInAssetBalance = sender.getWithCustomHeader(s"/assets/balance/$secondAddress", headerName, headerValue)
    (Json.parse(sponsorInfoInAssetBalance.getResponseBody) \ "balances" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(sponsorInfoInAssetBalance.getResponseBody) \ "balances" \ 0 \ "sponsorBalance").as[String] shouldBe s"${sender.balanceDetails(secondAddress).available}"

    val massTransferTxId = sender.massTransfer(firstAddress, List(Transfer(secondAddress, 1000)), calcMassTransferFee(1)).id
    val utxMassTransferTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$massTransferTxId", headerName, headerValue)
    (Json.parse(utxMassTransferTxInfo.getResponseBody) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(utxMassTransferTxInfo.getResponseBody) \ "totalAmount").as[String] shouldBe "1000"
    val utx4 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx4.getResponseBody) \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(utx4.getResponseBody) \ 0 \ "totalAmount").as[String] shouldBe "1000"
    sender.waitForTransaction(massTransferTxId)
    val massTransferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$massTransferTxId", headerName, headerValue)
    (Json.parse(massTransferTxInfo.getResponseBody) \ "transfers" \ 0 \ "amount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(massTransferTxInfo.getResponseBody) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(massTransferTxInfo.getResponseBody) \ "totalAmount").validate[JsString] shouldBe a[JsSuccess[_]]
    (Json.parse(massTransferTxInfo.getResponseBody) \ "totalAmount").as[String] shouldBe "1000"

    val tx =
      Json.obj(
        "type" -> CreateAliasTransaction.typeId,
        "sender" -> firstAddress,
        "alias" -> "alias",
        "fee" -> 100000,
        "timestamp" -> System.currentTimeMillis(),
        "version" -> 1,
        "senderPublicKey" -> PublicKey.fromBase58String(firstAddress).explicitGet()
      )
    val calculateTxFee = sender.postJsObjectWithCustomHeader("/transactions/calculateFee", tx, headerName, headerValue)
    (Json.parse(calculateTxFee.getResponseBody) \ "feeAmount").as[String] shouldBe s"$minFee"
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
    val currentHeight = sender.height
    val rewards         = sender.getWithCustomHeader("/blockchain/rewards", headerName, headerValue)
    val rewardsByHeight = sender.getWithCustomHeader(s"/blockchain/rewards/$currentHeight", headerName, headerValue)
    val rewardsAsInteger = sender.rewardStatus(currentHeight)
    (Json.parse(rewards.getResponseBody) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (Json.parse(rewards.getResponseBody) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (Json.parse(rewards.getResponseBody) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"

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
