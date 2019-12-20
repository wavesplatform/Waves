package com.wavesplatform.it.sync

import com.wavesplatform.account.{AddressOrAlias, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV2, Order}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import play.api.libs.json.Json

class AmountAsStringSuite extends BaseTransactionSuite {

  val (headerName, headerValue) = ("Accept", "application/json;large-significand-format=string")

  test("amount as string in assets api") {
    val assetId = sender.issue(firstAddress, "assetName", "description", someAssetAmount, 8, fee = issueFee, waitForTx = true).id
    val nftAssetId =
      sender.issue(firstAddress, "assetName", "description", quantity = 1, decimals = 0, reissuable = false, fee = issueFee, waitForTx = true).id
    val currentHeight   = sender.height
    val assetDetails    = sender.getWithCustomHeader(s"/assets/details/$assetId", headerName, headerValue)
    val assetBalance    = sender.getWithCustomHeader(s"/assets/balance/$firstAddress/$assetId", headerName, headerValue)
    val assetsBalance   = sender.getWithCustomHeader(s"/assets/balance/$firstAddress", headerName, headerValue)
    val nftBalance      = sender.getWithCustomHeader(s"/assets/nft/${firstAddress}/limit/1", headerName, headerValue)
    val assetBalanceInt = sender.assetBalance(firstAddress, assetId).balance

//    sender.transfer(firstAddress, secondAddress, transferAmount, minFee, Some(assetId), waitForTx = true)
    sender.waitForHeight(currentHeight + 1)
    val assetDistribution = sender.getWithCustomHeader(s"/assets/$assetId/distribution/$currentHeight/limit/1", headerName, headerValue)

    (Json.parse(assetBalance.getResponseBody) \ "balance").as[String] shouldBe s"$assetBalanceInt"
    (Json.parse(assetsBalance.getResponseBody) \ "balances" \ 0 \ "balance").as[String] shouldBe s"$assetBalanceInt"
    (Json.parse(nftBalance.getResponseBody) \ 0 \ "quantity").as[String] shouldBe "1"
    (Json.parse(nftBalance.getResponseBody) \ 0 \ "fee").as[String] shouldBe s"$issueFee"
    (Json.parse(assetDistribution.getResponseBody) \ "items" \ 0 \ 1).as[String] shouldBe s"$someAssetAmount"
    (Json.parse(assetDetails.getResponseBody) \ "quantity").as[String] shouldBe s"$someAssetAmount"
  }

  test("amount as string in addresses api") {
    val firstBalance = sender.balanceDetails(firstAddress)
    val balance      = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress", headerName, headerValue)
    (Json.parse(balance.getResponseBody) \ "balance").as[String] shouldBe s"${firstBalance.regular}"
    val balanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress/1", headerName, headerValue)
    (Json.parse(balanceWithConfirmations.getResponseBody) \ "balance").as[String] shouldBe s"${firstBalance.regular}"

    val balanceDetails = sender.getWithCustomHeader(s"/addresses/balance/details/$firstAddress", headerName, headerValue)
    (Json.parse(balanceDetails.getResponseBody) \ "regular").as[String] shouldBe s"${firstBalance.regular}"
    (Json.parse(balanceDetails.getResponseBody) \ "generating").as[String] shouldBe s"${firstBalance.generating}"
    (Json.parse(balanceDetails.getResponseBody) \ "available").as[String] shouldBe s"${firstBalance.available}"
    (Json.parse(balanceDetails.getResponseBody) \ "effective").as[String] shouldBe s"${firstBalance.effective}"

    val effectiveBalance                  = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress", headerName, headerValue)
    val effectiveBalanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress/1", headerName, headerValue)
    (Json.parse(effectiveBalance.getResponseBody) \ "balance").as[String] shouldBe s"${firstBalance.effective}"
    (Json.parse(effectiveBalanceWithConfirmations.getResponseBody) \ "balance").as[String] shouldBe s"${firstBalance.effective}"
  }

  test("amount as string in transactions api") {
    val exchanger      = KeyPair("exchanger".getBytes)
    val transferTxId   = sender.transfer(firstAddress, exchanger.stringRepr, transferAmount, minFee, waitForTx = true).id
    val transferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$transferTxId", headerName, headerValue)
    (Json.parse(transferTxInfo.getResponseBody) \ "amount").as[String] shouldBe s"$transferAmount"
    (Json.parse(transferTxInfo.getResponseBody) \ "fee").as[String] shouldBe s"$minFee"

    val amount = 1000000
    val price  = 1000
    val exchAssetId = sender
      .broadcastIssue(exchanger, "exchange asset", "", someAssetAmount, 8, fee = issueFee, reissuable = true, script = None, waitForTx = true)
      .id
    val ts = System.currentTimeMillis()
    val buyOrder = Order.buy(
      exchanger,
      exchanger.publicKey,
      AssetPair.createAssetPair("WAVES", exchAssetId).get,
      amount,
      price,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    val sellOrder = Order.sell(
      exchanger,
      exchanger.publicKey,
      AssetPair.createAssetPair("WAVES", exchAssetId).get,
      amount,
      price,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    nodes.waitForHeightArise()
    val exchangeTx              = ExchangeTransactionV2.create(exchanger, buyOrder, sellOrder, amount, price, matcherFee, matcherFee, matcherFee, ts).explicitGet()
    val exchangeTxId            = exchangeTx.id().base58
    val exchangeTxFromBroadcast = sender.postJsObjectWithCustomHeader("/transactions/broadcast", exchangeTx.json(), headerName, headerValue)
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "amount").as[String] shouldBe s"$amount"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "price").as[String] shouldBe s"$price"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxFromBroadcast.getResponseBody) \ "fee").as[String] shouldBe s"$matcherFee"

    val utxExchangeTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$exchangeTxId", headerName, headerValue)
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "amount").as[String] shouldBe s"$amount"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "price").as[String] shouldBe s"$price"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utxExchangeTxInfo.getResponseBody) \ "fee").as[String] shouldBe s"$matcherFee"
    val utx = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx.getResponseBody) \ 0 \ "amount").as[String] shouldBe s"$amount"
    (Json.parse(utx.getResponseBody) \ 0 \ "price").as[String] shouldBe s"$price"
    (Json.parse(utx.getResponseBody) \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utx.getResponseBody) \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utx.getResponseBody) \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utx.getResponseBody) \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(utx.getResponseBody) \ 0 \ "fee").as[String] shouldBe s"$matcherFee"
    val exchangeTxHeight    = sender.waitForTransaction(exchangeTxId).height
    val exchangeTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val exchangeTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$exchangeTxHeight", headerName, headerValue)
    val exchangeTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(exchangeTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(exchangeTxBlockLast, exchangeTxBlockAt, exchangeTxBlockBySignature)) {
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "amount").as[String] shouldBe s"$amount"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "price").as[String] shouldBe s"$price"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$matcherFee"
    }
    val blockSeq = sender.getWithCustomHeader(s"/blocks/seq/$exchangeTxHeight/$exchangeTxHeight", headerName, headerValue)
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "amount").as[String] shouldBe s"$amount"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "price").as[String] shouldBe s"$price"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(blockSeq.getResponseBody) \ 0 \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$matcherFee"

    val exchangeTxInfo = sender.getWithCustomHeader(s"/transactions/info/$exchangeTxId", headerName, headerValue)
    (Json.parse(exchangeTxInfo.getResponseBody) \ "amount").as[String] shouldBe s"$amount"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "price").as[String] shouldBe s"$price"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (Json.parse(exchangeTxInfo.getResponseBody) \ "fee").as[String] shouldBe s"$matcherFee"

    nodes.waitForHeightArise()
    val dataEntries         = List(IntegerDataEntry("int", 666))
    val dataFee             = calcDataFee(dataEntries)
    val dataTx              = DataTransaction.selfSigned(sender.privateKey, dataEntries, dataFee, System.currentTimeMillis()).explicitGet()
    val dataTxId            = dataTx.id().base58
    val dataTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", dataTx.json(), headerName, headerValue)
    (Json.parse(dataTxFromBroadcast.getResponseBody) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val utxDataTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$dataTxId", headerName, headerValue)
    (Json.parse(utxDataTxInfo.getResponseBody) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val utx2 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx2.getResponseBody) \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val dataTxHeight    = sender.waitForTransaction(dataTxId).height
    val dataTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val dataTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$dataTxHeight", headerName, headerValue)
    val dataTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(dataTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(dataTxBlockLast, dataTxBlockAt, dataTxBlockBySignature)) {
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    }
    val blockSeqWithDataTx = sender.getWithCustomHeader(s"/blocks/seq/$dataTxHeight/$dataTxHeight", headerName, headerValue)
    (Json.parse(blockSeqWithDataTx.getResponseBody) \ 0 \ "transactions" \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val dataTxInfo = sender.getWithCustomHeader(s"/transactions/info/$dataTxId", headerName, headerValue)
    (Json.parse(dataTxInfo.getResponseBody) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val addressData = sender.getWithCustomHeader(s"/addresses/data/${sender.address}", headerName, headerValue)
    (Json.parse(addressData.getResponseBody) \ 0 \ "value").as[String] shouldBe "666"

    val sponsoredAssetId = sender.issue(sender.address, "sponsor", "", someAssetAmount, 8, waitForTx = true).id
    nodes.waitForHeightArise()
    val sponsorshipTx = SponsorFeeTransaction
      .selfSigned(sender.privateKey, IssuedAsset(ByteStr.decodeBase58(sponsoredAssetId).get), Some(10000), sponsorFee, System.currentTimeMillis())
      .explicitGet()
    val sponsorshipTxId            = sponsorshipTx.id().base58
    val sponsorshipTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", sponsorshipTx.json(), headerName, headerValue)
    (Json.parse(sponsorshipTxFromBroadcast.getResponseBody) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(sponsorshipTxFromBroadcast.getResponseBody) \ "fee").as[String] shouldBe s"$sponsorFee"
    val utxSponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$sponsorshipTxId", headerName, headerValue)
    (Json.parse(utxSponsorshipTxInfo.getResponseBody) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(utxSponsorshipTxInfo.getResponseBody) \ "fee").as[String] shouldBe s"$sponsorFee"
    val utx3 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx3.getResponseBody) \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(utx3.getResponseBody) \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorshipTxHeight    = sender.waitForTransaction(sponsorshipTxId).height
    val sponsorshipTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val sponsorshipTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$sponsorshipTxHeight", headerName, headerValue)
    val sponsorshipTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(sponsorshipTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(sponsorshipTxBlockLast, sponsorshipTxBlockAt, sponsorshipTxBlockBySignature)) {
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    }
    val blockSeqWithSponsorshipTx = sender.getWithCustomHeader(s"/blocks/seq/$sponsorshipTxHeight/$sponsorshipTxHeight", headerName, headerValue)
    (Json.parse(blockSeqWithSponsorshipTx.getResponseBody) \ 0 \ "transactions" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(blockSeqWithSponsorshipTx.getResponseBody) \ 0 \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/info/$sponsorshipTxId", headerName, headerValue)
    (Json.parse(sponsorshipTxInfo.getResponseBody) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(sponsorshipTxInfo.getResponseBody) \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorInfoInAssetBalance = sender.getWithCustomHeader(s"/assets/balance/${sender.address}", headerName, headerValue)
    (Json.parse(sponsorInfoInAssetBalance.getResponseBody) \ "balances" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (Json.parse(sponsorInfoInAssetBalance.getResponseBody) \ "balances" \ 0 \ "sponsorBalance")
      .as[String] shouldBe s"${sender.balanceDetails(sender.address).available}"

    nodes.waitForHeightArise()
    val massTransferTx = MassTransferTransaction
      .selfSigned(
        Waves,
        sender.privateKey,
        List(ParsedTransfer(AddressOrAlias.fromString(secondAddress).explicitGet(), 1000)),
        System.currentTimeMillis(),
        calcMassTransferFee(1),
        Array.emptyByteArray
      )
      .explicitGet()
    val massTransferTxId            = massTransferTx.id().base58
    val massTransferTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", massTransferTx.json(), headerName, headerValue)
    (Json.parse(massTransferTxFromBroadcast.getResponseBody) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(massTransferTxFromBroadcast.getResponseBody) \ "totalAmount").as[String] shouldBe "1000"
    val utxMassTransferTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$massTransferTxId", headerName, headerValue)
    (Json.parse(utxMassTransferTxInfo.getResponseBody) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(utxMassTransferTxInfo.getResponseBody) \ "totalAmount").as[String] shouldBe "1000"
    val utx4 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (Json.parse(utx4.getResponseBody) \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(utx4.getResponseBody) \ 0 \ "totalAmount").as[String] shouldBe "1000"
    val massTransferTxHeight    = sender.waitForTransaction(massTransferTxId).height
    val massTransferTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val massTransferTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$massTransferTxHeight", headerName, headerValue)
    val massTransferTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(massTransferTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(massTransferTxBlockLast, massTransferTxBlockAt, massTransferTxBlockBySignature)) {
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
      (Json.parse(block.getResponseBody) \ "transactions" \ 0 \ "totalAmount").as[String] shouldBe "1000"
    }
    val blockSeqWithMassTransferTx = sender.getWithCustomHeader(s"/blocks/seq/$massTransferTxHeight/$massTransferTxHeight", headerName, headerValue)
    (Json.parse(blockSeqWithMassTransferTx.getResponseBody) \ 0 \ "transactions" \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(blockSeqWithMassTransferTx.getResponseBody) \ 0 \ "transactions" \ 0 \ "totalAmount").as[String] shouldBe "1000"
    val massTransferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$massTransferTxId", headerName, headerValue)
    (Json.parse(massTransferTxInfo.getResponseBody) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (Json.parse(massTransferTxInfo.getResponseBody) \ "totalAmount").as[String] shouldBe "1000"

    val tx =
      Json.obj(
        "type"            -> CreateAliasTransaction.typeId,
        "sender"          -> firstAddress,
        "alias"           -> "alias",
        "fee"             -> 100000,
        "timestamp"       -> System.currentTimeMillis(),
        "version"         -> 1,
        "senderPublicKey" -> PublicKey.fromBase58String(firstAddress).explicitGet()
      )
    val calculateTxFee = sender.postJsObjectWithCustomHeader("/transactions/calculateFee", tx, headerName, headerValue)
    (Json.parse(calculateTxFee.getResponseBody) \ "feeAmount").as[String] shouldBe s"$minFee"
  }

  test("amount as string in blocks api") {
    nodes.waitForHeightArise()
    val currentHeight = sender.height
    val blockLast        = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val blockAt          = sender.getWithCustomHeader(s"/blocks/at/$currentHeight", headerName, headerValue)
    val blockBySignature = sender.getWithCustomHeader(s"/blocks/signature/${sender.lastBlock.signature}", headerName, headerValue)
    val blockHeadersLast = sender.getWithCustomHeader("/blocks/headers/last", headerName, headerValue)
    val blockHeadersAt   = sender.getWithCustomHeader(s"/blocks/headers/at/$currentHeight", headerName, headerValue)
    val reward = sender.rewardStatus(currentHeight).currentReward

    for (block <- Seq(blockLast, blockAt, blockHeadersLast, blockHeadersAt, blockBySignature)) {
      (Json.parse(block.getResponseBody) \ "reward").as[String] shouldBe s"$reward"
      (Json.parse(block.getResponseBody) \ "desiredReward").as[String] shouldBe "-1"
      (Json.parse(block.getResponseBody) \ "totalFee").as[String] shouldBe "0"
    }

    val blockSeq        = sender.getWithCustomHeader(s"/blocks/seq/$currentHeight/$currentHeight", headerName, headerValue)
    val blockHeadersSeq = sender.getWithCustomHeader(s"/blocks/headers/seq/$currentHeight/$currentHeight", headerName, headerValue)
    val blockSeqByAddress =
      sender.getWithCustomHeader(s"/blocks/address/${sender.address}/$currentHeight/$currentHeight", headerName, headerValue)

    for (block <- Seq(blockSeq, blockHeadersSeq, blockSeqByAddress)) {
      (Json.parse(block.getResponseBody) \ 0 \ "reward").as[String] shouldBe s"$reward"
      (Json.parse(block.getResponseBody) \ 0 \ "desiredReward").as[String] shouldBe "-1"
      (Json.parse(block.getResponseBody) \ 0 \ "totalFee").as[String] shouldBe "0"
    }
  }

  test("amount as string in rewards api") {
    val currentHeight    = sender.height
    val rewards          = sender.getWithCustomHeader("/blockchain/rewards", headerName, headerValue)
    val rewardsByHeight  = sender.getWithCustomHeader(s"/blockchain/rewards/$currentHeight", headerName, headerValue)
    val rewardsAsInteger = sender.rewardStatus(currentHeight)
    (Json.parse(rewards.getResponseBody) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (Json.parse(rewards.getResponseBody) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (Json.parse(rewards.getResponseBody) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (Json.parse(rewardsByHeight.getResponseBody) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"

  }

  test("amount as string in consensus api") {
    val firstGeneratingBalance = sender.balanceDetails(firstAddress).generating
    val generatingBalance      = sender.getWithCustomHeader(s"/consensus/generatingbalance/$firstAddress", headerName, headerValue)
    (Json.parse(generatingBalance.getResponseBody) \ "balance").as[String] shouldBe s"$firstGeneratingBalance"
  }

  test("amount as string in debug api") {
    val firstBalance = sender.balanceDetails(firstAddress).available
    val portfolio    = sender.getWithCustomHeader(s"/debug/portfolios/$firstAddress", headerName, headerValue, withApiKey = true)
    (Json.parse(portfolio.getResponseBody) \ "balance").as[String] shouldBe s"$firstBalance"
    (Json.parse(portfolio.getResponseBody) \ "lease" \ "in").as[String] shouldBe "0"
    (Json.parse(portfolio.getResponseBody) \ "lease" \ "out").as[String] shouldBe "0"

    val state = sender.getWithCustomHeader("/debug/state", headerName, headerValue, withApiKey = true)
    (Json.parse(state.getResponseBody) \ s"$firstAddress").as[String] shouldBe s"$firstBalance"

    val balanceHistory = sender.getWithCustomHeader(s"/debug/balances/history/$firstAddress", headerName, headerValue, withApiKey = true)
    (Json.parse(balanceHistory.getResponseBody) \ 0 \ "balance").as[String] shouldBe s"$firstBalance"

    val stateWavesOnHeight = sender.getWithCustomHeader(s"/debug/stateWaves/${sender.height}", headerName, headerValue, withApiKey = true)
    (Json.parse(stateWavesOnHeight.getResponseBody) \ s"$firstAddress").as[String] shouldBe s"$firstBalance"
  }

}
