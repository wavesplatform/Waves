package com.wavesplatform.it.sync

import com.wavesplatform.account.{AddressOrAlias, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.transfer.{Attachment, MassTransferTransaction}
import org.asynchttpclient.Response
import play.api.libs.json.{JsValue, Json}

class AmountAsStringSuite extends BaseTransactionSuite {

  val (headerName, headerValue) = ("Accept", "application/json;large-significand-format=string")

  test("amount as string in assets api") {
    val assetId = sender.issue(firstAddress, "assetName", "description", someAssetAmount, 8, fee = issueFee, waitForTx = true).id
    sender.issue(firstAddress, "assetName", "description", quantity = 1, decimals = 0, reissuable = false, fee = issueFee, waitForTx = true).id
    val currentHeight   = sender.height
    val assetDetails    = sender.getWithCustomHeader(s"/assets/details/$assetId", headerName, headerValue)
    val assetBalance    = sender.getWithCustomHeader(s"/assets/balance/$firstAddress/$assetId", headerName, headerValue)
    val assetsBalance   = sender.getWithCustomHeader(s"/assets/balance/$firstAddress", headerName, headerValue)
    val nftBalance      = sender.getWithCustomHeader(s"/assets/nft/${firstAddress}/limit/1", headerName, headerValue)
    val assetBalanceInt = sender.assetBalance(firstAddress, assetId).balance

    sender.waitForHeight(currentHeight + 1)
    val assetDistribution = sender.getWithCustomHeader(s"/assets/$assetId/distribution/$currentHeight/limit/1", headerName, headerValue)

    (parseResponse(assetBalance) \ "balance").as[String] shouldBe s"$assetBalanceInt"
    (parseResponse(assetsBalance) \ "balances" \ 0 \ "balance").as[String] shouldBe s"$assetBalanceInt"
    (parseResponse(nftBalance) \ 0 \ "quantity").as[String] shouldBe "1"
    (parseResponse(assetDistribution) \ "items" \ 0 \ 1).as[String] shouldBe s"$someAssetAmount"
    (parseResponse(assetDetails) \ "quantity").as[String] shouldBe s"$someAssetAmount"
  }

  test("amount as string in addresses api") {
    val firstBalance = sender.balanceDetails(firstAddress)
    val balance      = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress", headerName, headerValue)
    (parseResponse(balance) \ "balance").as[String] shouldBe s"${firstBalance.regular}"
    val balanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/balance/$firstAddress/1", headerName, headerValue)
    (parseResponse(balanceWithConfirmations) \ "balance").as[String] shouldBe s"${firstBalance.regular}"

    val balanceDetails = sender.getWithCustomHeader(s"/addresses/balance/details/$firstAddress", headerName, headerValue)
    (parseResponse(balanceDetails) \ "regular").as[String] shouldBe s"${firstBalance.regular}"
    (parseResponse(balanceDetails) \ "generating").as[String] shouldBe s"${firstBalance.generating}"
    (parseResponse(balanceDetails) \ "available").as[String] shouldBe s"${firstBalance.available}"
    (parseResponse(balanceDetails) \ "effective").as[String] shouldBe s"${firstBalance.effective}"

    val effectiveBalance                  = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress", headerName, headerValue)
    val effectiveBalanceWithConfirmations = sender.getWithCustomHeader(s"/addresses/effectiveBalance/$firstAddress/1", headerName, headerValue)
    (parseResponse(effectiveBalance) \ "balance").as[String] shouldBe s"${firstBalance.effective}"
    (parseResponse(effectiveBalanceWithConfirmations) \ "balance").as[String] shouldBe s"${firstBalance.effective}"
  }

  test("amount as string in exchange transaction") {
    val exchanger      = KeyPair("exchanger".getBytes)
    val transferTxId   = sender.transfer(firstAddress, exchanger.stringRepr, transferAmount, minFee, waitForTx = true).id
    val transferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$transferTxId", headerName, headerValue)
    (parseResponse(transferTxInfo) \ "amount").as[String] shouldBe s"$transferAmount"
    (parseResponse(transferTxInfo) \ "fee").as[String] shouldBe s"$minFee"

    val amount = 1000000
    val price  = 1000
    val exchAssetId = sender
      .broadcastIssue(exchanger, "exchange asset", "", someAssetAmount, 8, fee = issueFee, reissuable = true, script = None, waitForTx = true)
      .id
    val ts = System.currentTimeMillis()
    val buyOrder = Order.buy(
      version = TxVersion.V2,
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
      version = TxVersion.V2,
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
    val exchangeTx =
      ExchangeTransaction.signed(TxVersion.V2, exchanger, buyOrder, sellOrder, amount, price, matcherFee, matcherFee, matcherFee, ts).explicitGet()
    val exchangeTxId            = Base58.encode(exchangeTx.id())
    val exchangeTxFromBroadcast = sender.postJsObjectWithCustomHeader("/transactions/broadcast", exchangeTx.json(), headerName, headerValue)
    (parseResponse(exchangeTxFromBroadcast) \ "amount").as[String] shouldBe s"$amount"
    (parseResponse(exchangeTxFromBroadcast) \ "price").as[String] shouldBe s"$price"
    (parseResponse(exchangeTxFromBroadcast) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxFromBroadcast) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxFromBroadcast) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxFromBroadcast) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxFromBroadcast) \ "fee").as[String] shouldBe s"$matcherFee"

    val utxExchangeTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$exchangeTxId", headerName, headerValue)
    (parseResponse(utxExchangeTxInfo) \ "amount").as[String] shouldBe s"$amount"
    (parseResponse(utxExchangeTxInfo) \ "price").as[String] shouldBe s"$price"
    (parseResponse(utxExchangeTxInfo) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utxExchangeTxInfo) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utxExchangeTxInfo) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utxExchangeTxInfo) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utxExchangeTxInfo) \ "fee").as[String] shouldBe s"$matcherFee"
    val utx = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (parseResponse(utx) \ 0 \ "amount").as[String] shouldBe s"$amount"
    (parseResponse(utx) \ 0 \ "price").as[String] shouldBe s"$price"
    (parseResponse(utx) \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utx) \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utx) \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utx) \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(utx) \ 0 \ "fee").as[String] shouldBe s"$matcherFee"
    val exchangeTxHeight    = sender.waitForTransaction(exchangeTxId).height
    val exchangeTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val exchangeTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$exchangeTxHeight", headerName, headerValue)
    val exchangeTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(exchangeTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(exchangeTxBlockLast, exchangeTxBlockAt, exchangeTxBlockBySignature)) {
      (parseResponse(block) \ "transactions" \ 0 \ "amount").as[String] shouldBe s"$amount"
      (parseResponse(block) \ "transactions" \ 0 \ "price").as[String] shouldBe s"$price"
      (parseResponse(block) \ "transactions" \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
      (parseResponse(block) \ "transactions" \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
      (parseResponse(block) \ "transactions" \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
      (parseResponse(block) \ "transactions" \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
      (parseResponse(block) \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$matcherFee"
    }
    val blockSeq = sender.getWithCustomHeader(s"/blocks/seq/$exchangeTxHeight/$exchangeTxHeight", headerName, headerValue)
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "amount").as[String] shouldBe s"$amount"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "price").as[String] shouldBe s"$price"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(blockSeq) \ 0 \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$matcherFee"

    val exchangeTxInfo = sender.getWithCustomHeader(s"/transactions/info/$exchangeTxId", headerName, headerValue)
    (parseResponse(exchangeTxInfo) \ "amount").as[String] shouldBe s"$amount"
    (parseResponse(exchangeTxInfo) \ "price").as[String] shouldBe s"$price"
    (parseResponse(exchangeTxInfo) \ "sellMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxInfo) \ "buyMatcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxInfo) \ "order1" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxInfo) \ "order2" \ "matcherFee").as[String] shouldBe s"$matcherFee"
    (parseResponse(exchangeTxInfo) \ "fee").as[String] shouldBe s"$matcherFee"
  }

  test("amount as string in data transaction") {
    nodes.waitForHeightArise()
    val dataEntries         = List(IntegerDataEntry("int", 666))
    val dataFee             = calcDataFee(dataEntries)
    val dataTx              = DataTransaction.selfSigned(TxVersion.V2, sender.privateKey, dataEntries, dataFee, System.currentTimeMillis()).explicitGet()
    val dataTxId            = Base58.encode(dataTx.id())
    val dataTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", dataTx.json(), headerName, headerValue)
    (parseResponse(dataTxFromBroadcast) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val utxDataTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$dataTxId", headerName, headerValue)
    (parseResponse(utxDataTxInfo) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val utx2 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (parseResponse(utx2) \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val dataTxHeight    = sender.waitForTransaction(dataTxId).height
    val dataTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val dataTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$dataTxHeight", headerName, headerValue)
    val dataTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(dataTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(dataTxBlockLast, dataTxBlockAt, dataTxBlockBySignature)) {
      (parseResponse(block) \ "transactions" \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    }
    val blockSeqWithDataTx = sender.getWithCustomHeader(s"/blocks/seq/$dataTxHeight/$dataTxHeight", headerName, headerValue)
    (parseResponse(blockSeqWithDataTx) \ 0 \ "transactions" \ 0 \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val dataTxInfo = sender.getWithCustomHeader(s"/transactions/info/$dataTxId", headerName, headerValue)
    (parseResponse(dataTxInfo) \ "data" \ 0 \ "value").as[String] shouldBe "666"
    val addressData = sender.getWithCustomHeader(s"/addresses/data/${sender.address}", headerName, headerValue)
    (parseResponse(addressData) \ 0 \ "value").as[String] shouldBe "666"
  }

  test("amount as string in sponsorfee transaction") {
    val sponsoredAssetId = sender.issue(sender.address, "sponsor", "", someAssetAmount, 8, waitForTx = true).id
    nodes.waitForHeightArise()
    val sponsorshipTx = SponsorFeeTransaction
      .selfSigned(
        TxVersion.V2,
        sender.privateKey,
        IssuedAsset(ByteStr.decodeBase58(sponsoredAssetId).get),
        Some(10000),
        sponsorFee,
        System.currentTimeMillis()
      )
      .explicitGet()
    val sponsorshipTxId            = Base58.encode(sponsorshipTx.id())
    val sponsorshipTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", sponsorshipTx.json(), headerName, headerValue)
    (parseResponse(sponsorshipTxFromBroadcast) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(sponsorshipTxFromBroadcast) \ "fee").as[String] shouldBe s"$sponsorFee"
    val utxSponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$sponsorshipTxId", headerName, headerValue)
    (parseResponse(utxSponsorshipTxInfo) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(utxSponsorshipTxInfo) \ "fee").as[String] shouldBe s"$sponsorFee"
    val utx3 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (parseResponse(utx3) \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(utx3) \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorshipTxHeight    = sender.waitForTransaction(sponsorshipTxId).height
    val sponsorshipTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val sponsorshipTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$sponsorshipTxHeight", headerName, headerValue)
    val sponsorshipTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(sponsorshipTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(sponsorshipTxBlockLast, sponsorshipTxBlockAt, sponsorshipTxBlockBySignature)) {
      (parseResponse(block) \ "transactions" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
      (parseResponse(block) \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    }
    val blockSeqWithSponsorshipTx = sender.getWithCustomHeader(s"/blocks/seq/$sponsorshipTxHeight/$sponsorshipTxHeight", headerName, headerValue)
    (parseResponse(blockSeqWithSponsorshipTx) \ 0 \ "transactions" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(blockSeqWithSponsorshipTx) \ 0 \ "transactions" \ 0 \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorshipTxInfo = sender.getWithCustomHeader(s"/transactions/info/$sponsorshipTxId", headerName, headerValue)
    (parseResponse(sponsorshipTxInfo) \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(sponsorshipTxInfo) \ "fee").as[String] shouldBe s"$sponsorFee"
    val sponsorInfoInAssetBalance = sender.getWithCustomHeader(s"/assets/balance/${sender.address}", headerName, headerValue)
    (parseResponse(sponsorInfoInAssetBalance) \ "balances" \ 0 \ "minSponsoredAssetFee").as[String] shouldBe "10000"
    (parseResponse(sponsorInfoInAssetBalance) \ "balances" \ 0 \ "sponsorBalance")
      .as[String] shouldBe s"${sender.balanceDetails(sender.address).available}"
  }
  test("amount as string in masstransfer transaction") {
    nodes.waitForHeightArise()
    val massTransferTx = MassTransferTransaction
      .selfSigned(
        TxVersion.V2,
        sender.privateKey,
        Waves,
        List(ParsedTransfer(AddressOrAlias.fromString(secondAddress).explicitGet(), 1000)),
        calcMassTransferFee(1),
        System.currentTimeMillis(),
        None
      )
      .explicitGet()
    val massTransferTxId            = Base58.encode(massTransferTx.id())
    val massTransferTxFromBroadcast = sender.postJsObjectWithCustomHeader(s"/transactions/broadcast", massTransferTx.json(), headerName, headerValue)
    (parseResponse(massTransferTxFromBroadcast) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (parseResponse(massTransferTxFromBroadcast) \ "totalAmount").as[String] shouldBe "1000"
    val utxMassTransferTxInfo = sender.getWithCustomHeader(s"/transactions/unconfirmed/info/$massTransferTxId", headerName, headerValue)
    (parseResponse(utxMassTransferTxInfo) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (parseResponse(utxMassTransferTxInfo) \ "totalAmount").as[String] shouldBe "1000"
    val utx4 = sender.getWithCustomHeader(s"/transactions/unconfirmed", headerName, headerValue)
    (parseResponse(utx4) \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (parseResponse(utx4) \ 0 \ "totalAmount").as[String] shouldBe "1000"
    val massTransferTxHeight    = sender.waitForTransaction(massTransferTxId).height
    val massTransferTxBlockLast = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val massTransferTxBlockAt   = sender.getWithCustomHeader(s"/blocks/at/$massTransferTxHeight", headerName, headerValue)
    val massTransferTxBlockBySignature =
      sender.getWithCustomHeader(s"/blocks/signature/${sender.blockAt(massTransferTxHeight).signature}", headerName, headerValue)
    for (block <- Seq(massTransferTxBlockLast, massTransferTxBlockAt, massTransferTxBlockBySignature)) {
      (parseResponse(block) \ "transactions" \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
      (parseResponse(block) \ "transactions" \ 0 \ "totalAmount").as[String] shouldBe "1000"
    }
    val blockSeqWithMassTransferTx = sender.getWithCustomHeader(s"/blocks/seq/$massTransferTxHeight/$massTransferTxHeight", headerName, headerValue)
    (parseResponse(blockSeqWithMassTransferTx) \ 0 \ "transactions" \ 0 \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (parseResponse(blockSeqWithMassTransferTx) \ 0 \ "transactions" \ 0 \ "totalAmount").as[String] shouldBe "1000"
    val massTransferTxInfo = sender.getWithCustomHeader(s"/transactions/info/$massTransferTxId", headerName, headerValue)
    (parseResponse(massTransferTxInfo) \ "transfers" \ 0 \ "amount").as[String] shouldBe "1000"
    (parseResponse(massTransferTxInfo) \ "totalAmount").as[String] shouldBe "1000"

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
    (parseResponse(calculateTxFee) \ "feeAmount").as[String] shouldBe s"$minFee"
  }

  test("amount as string in blocks api") {
    nodes.waitForHeightArise()
    val currentHeight    = sender.height
    val blockLast        = sender.getWithCustomHeader("/blocks/last", headerName, headerValue)
    val blockAt          = sender.getWithCustomHeader(s"/blocks/at/$currentHeight", headerName, headerValue)
    val blockBySignature = sender.getWithCustomHeader(s"/blocks/signature/${sender.lastBlock.signature}", headerName, headerValue)
    val blockHeadersLast = sender.getWithCustomHeader("/blocks/headers/last", headerName, headerValue)
    val blockHeadersAt   = sender.getWithCustomHeader(s"/blocks/headers/at/$currentHeight", headerName, headerValue)
    val reward           = sender.rewardStatus(currentHeight).currentReward

    for (block <- Seq(blockLast, blockAt, blockHeadersLast, blockHeadersAt, blockBySignature)) {
      (parseResponse(block) \ "reward").as[String] shouldBe s"$reward"
      (parseResponse(block) \ "desiredReward").as[String] shouldBe "-1"
      (parseResponse(block) \ "totalFee").as[String] shouldBe "0"
    }

    val blockSeq        = sender.getWithCustomHeader(s"/blocks/seq/$currentHeight/$currentHeight", headerName, headerValue)
    val blockHeadersSeq = sender.getWithCustomHeader(s"/blocks/headers/seq/$currentHeight/$currentHeight", headerName, headerValue)
    val blockSeqByAddress =
      sender.getWithCustomHeader(s"/blocks/address/${sender.address}/$currentHeight/$currentHeight", headerName, headerValue)

    for (block <- Seq(blockSeq, blockHeadersSeq, blockSeqByAddress)) {
      (parseResponse(block) \ 0 \ "reward").as[String] shouldBe s"$reward"
      (parseResponse(block) \ 0 \ "desiredReward").as[String] shouldBe "-1"
      (parseResponse(block) \ 0 \ "totalFee").as[String] shouldBe "0"
    }
  }

  test("amount as string in rewards api") {
    val currentHeight    = sender.height
    val rewards          = sender.getWithCustomHeader("/blockchain/rewards", headerName, headerValue)
    val rewardsByHeight  = sender.getWithCustomHeader(s"/blockchain/rewards/$currentHeight", headerName, headerValue)
    val rewardsAsInteger = sender.rewardStatus(currentHeight)
    (parseResponse(rewards) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (parseResponse(rewards) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (parseResponse(rewards) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"
    (parseResponse(rewardsByHeight) \ "totalWavesAmount").as[String] shouldBe s"${rewardsAsInteger.totalWavesAmount}"
    (parseResponse(rewardsByHeight) \ "currentReward").as[String] shouldBe s"${rewardsAsInteger.currentReward}"
    (parseResponse(rewardsByHeight) \ "minIncrement").as[String] shouldBe s"${rewardsAsInteger.minIncrement}"

  }

  test("amount as string in consensus api") {
    val firstGeneratingBalance = sender.balanceDetails(firstAddress).generating
    val generatingBalance      = sender.getWithCustomHeader(s"/consensus/generatingbalance/$firstAddress", headerName, headerValue)
    (parseResponse(generatingBalance) \ "balance").as[String] shouldBe s"$firstGeneratingBalance"
  }

  test("amount as string in debug api") {
    val firstBalance = sender.balanceDetails(firstAddress).available
    val portfolio    = sender.getWithCustomHeader(s"/debug/portfolios/$firstAddress", headerName, headerValue, withApiKey = true)
    (parseResponse(portfolio) \ "balance").as[String] shouldBe s"$firstBalance"
    (parseResponse(portfolio) \ "lease" \ "in").as[String] shouldBe "0"
    (parseResponse(portfolio) \ "lease" \ "out").as[String] shouldBe "0"

    val state = sender.getWithCustomHeader("/debug/state", headerName, headerValue, withApiKey = true)
    (parseResponse(state) \ s"$firstAddress").as[String] shouldBe s"$firstBalance"

    val balanceHistory = sender.getWithCustomHeader(s"/debug/balances/history/$firstAddress", headerName, headerValue, withApiKey = true)
    (parseResponse(balanceHistory) \ 0 \ "balance").as[String] shouldBe s"$firstBalance"

    val stateWavesOnHeight = sender.getWithCustomHeader(s"/debug/stateWaves/${sender.height}", headerName, headerValue, withApiKey = true)
    (parseResponse(stateWavesOnHeight) \ s"$firstAddress").as[String] shouldBe s"$firstBalance"
  }

  private def parseResponse(response: Response): JsValue = Json.parse(response.getResponseBody)
}
