package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.RandomKeyPair
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync.transactions.OverflowBlock
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.{CreateAliasTransaction, TxVersion}
import org.asynchttpclient.Response
import org.scalatest
import org.scalatest.Assertion
import play.api.libs.json.{JsString, JsValue, Json}

class AmountAsStringSuite extends BaseTransactionSuite with OverflowBlock {

  val (headerName, headerValue) = ("Accept", "application/json;large-significand-format=string")

  private lazy val testKP      = RandomKeyPair()
  private lazy val testAddress = testKP.toAddress.toString

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    miner.transfer(miner.keyPair, testAddress, 100.waves, minFee, None, None, waitForTx = true)
  }

  test("amount as string in assets api") {
    val assetId = miner.issue(testKP, "assetName", "description", someAssetAmount, 8, fee = issueFee, waitForTx = true).id
    miner.issue(testKP, "assetName", "description", quantity = 1, decimals = 0, reissuable = false, fee = issueFee, waitForTx = true)

    val currentHeight = miner.height
    miner.assetsDetails(assetId, amountsAsStrings = true).quantity shouldBe someAssetAmount
    miner.assetBalance(testAddress, assetId, amountsAsStrings = true).balance shouldBe someAssetAmount
    miner.portfolio(testAddress, amountsAsStrings = true).balances.head.balance shouldBe someAssetAmount
    miner.nftList(testAddress, 1, amountsAsStrings = true).head.quantity shouldBe 1

    miner.waitForHeight(currentHeight + 1)
    val assetDistribution = miner.getWithCustomHeader(
      s"/assets/$assetId/distribution/$currentHeight/limit/1",
      headerValue = "application/json;large-significand-format=string"
    )
    (parseResponse(assetDistribution) \ "items" \ testAddress).get shouldBe JsString(someAssetAmount.toString)
  }

  test("amount as string in addresses api") {
    val firstBalance = miner.balanceDetails(testAddress)
    miner.wavesBalance(testAddress, amountsAsStrings = true) shouldBe firstBalance.regular

    val balanceDetails = miner.balanceDetails(testAddress, amountsAsStrings = true)
    balanceDetails.regular shouldBe firstBalance.regular
    balanceDetails.generating shouldBe firstBalance.generating
    balanceDetails.available shouldBe firstBalance.available
    balanceDetails.effective shouldBe firstBalance.effective

    miner.effectiveBalance(testAddress, amountsAsStrings = true).balance shouldBe firstBalance.effective
    miner.effectiveBalance(testAddress, confirmations = Some(1), amountsAsStrings = true).balance shouldBe firstBalance.effective
  }

  test("amount as string in exchange transaction") {
    val exchanger      = KeyPair("exchanger".getBytes)
    val transferTxId   = miner.transfer(firstKeyPair, exchanger.toAddress.toString, transferAmount, minFee, waitForTx = true).id
    val transferTxInfo = miner.transactionInfo[TransactionInfo](transferTxId, amountsAsStrings = true)
    transferTxInfo.amount shouldBe Some(transferAmount)
    transferTxInfo.fee shouldBe minFee

    val amount = 1000000
    val price  = 1000
    def checkExchangeTx(exchangeTx: Transaction): scalatest.Assertion = {
      exchangeTx.amount shouldBe Some(amount)
      exchangeTx.price shouldBe Some(price)
      exchangeTx.sellMatcherFee shouldBe Some(matcherFee)
      exchangeTx.buyMatcherFee shouldBe Some(matcherFee)
      exchangeTx.sellOrderMatcherFee shouldBe Some(matcherFee)
      exchangeTx.buyOrderMatcherFee shouldBe Some(matcherFee)
      exchangeTx.fee shouldBe matcherFee
    }
    val exchAssetId = miner
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
      miner.broadcastExchange(exchanger, buyOrder, sellOrder, amount, price, matcherFee, matcherFee, matcherFee, amountsAsStrings = true)
    checkExchangeTx(exchangeTx)

    val utxExchangeTxInfoById = miner.utxById(exchangeTx.id, amountsAsStrings = true)
    val utxExchangeTxInfo     = miner.utx(amountsAsStrings = true)
    checkExchangeTx(utxExchangeTxInfoById)
    checkExchangeTx(utxExchangeTxInfo.head)

    val exchangeTxHeight           = miner.waitForTransaction(exchangeTx.id).height
    val exchangeTxBlockLast        = miner.lastBlock(amountsAsStrings = true).transactions.head
    val exchangeTxBlockAt          = miner.blockAt(exchangeTxHeight, amountsAsStrings = true).transactions.head
    val exchangeTxBlockBySignature = miner.blockById(miner.blockAt(exchangeTxHeight).id, amountsAsStrings = true).transactions.head
    val exchangeTxBlockSeq         = miner.blockSeq(exchangeTxHeight, exchangeTxHeight, amountsAsStrings = true).head.transactions.head
    checkExchangeTx(exchangeTxBlockLast)
    checkExchangeTx(exchangeTxBlockAt)
    checkExchangeTx(exchangeTxBlockBySignature)
    checkExchangeTx(exchangeTxBlockSeq)

    val exchangeTxInfo = miner.transactionInfo[TransactionInfo](exchangeTx.id, amountsAsStrings = true)
    exchangeTxInfo.amount shouldBe Some(amount)
    exchangeTxInfo.price shouldBe Some(price)
    exchangeTxInfo.sellMatcherFee shouldBe Some(matcherFee)
    exchangeTxInfo.buyMatcherFee shouldBe Some(matcherFee)
    exchangeTxInfo.sellOrderMatcherFee shouldBe Some(matcherFee)
    exchangeTxInfo.buyOrderMatcherFee shouldBe Some(matcherFee)
    exchangeTxInfo.fee shouldBe matcherFee
  }

  test("amount as string in data transaction") {
    nodes.waitForHeightArise()
    val dataEntries = List(IntegerDataEntry("int", 666))
    val dataFee     = calcDataFee(dataEntries, TxVersion.V1)
    val dataTx      = miner.putData(miner.keyPair, dataEntries, dataFee, amountsAsStrings = true)
    dataTx.fee shouldBe dataFee
    dataTx.data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)

    miner.utx(amountsAsStrings = true).head.data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)
    miner.utxById(dataTx.id, amountsAsStrings = true).data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)

    val dataTxHeight = miner.waitForTransaction(dataTx.id).height
    miner.lastBlock(amountsAsStrings = true).transactions.head.data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)
    miner.blockAt(dataTxHeight, amountsAsStrings = true).transactions.head.data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)
    miner
      .blockById(miner.lastBlock().id, amountsAsStrings = true)
      .transactions
      .head
      .data
      .map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)
    miner
      .blockSeq(dataTxHeight, dataTxHeight, amountsAsStrings = true)
      .head
      .transactions
      .head
      .data
      .map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)

    miner.transactionInfo[TransactionInfo](dataTx.id, amountsAsStrings = true).data.map(d => d.filter(_.key == "int").head.value) shouldBe Some(666)
    miner.getData(miner.address, amountsAsStrings = true).filter(_.key == "int").head.value shouldBe 666
  }

  test("amount as string in sponsorfee transaction") {
    def checkSponsorshipTx(tx: Transaction): Assertion = {
      tx.minSponsoredAssetFee shouldBe Some(10000)
      tx.fee shouldBe sponsorFee
    }
    val sponsoredAssetId = miner.issue(miner.keyPair, "sponsor", "", someAssetAmount, 8, waitForTx = true).id
    nodes.waitForHeightArise()
    val sponsorshipTx = miner.sponsorAsset(miner.keyPair, sponsoredAssetId, 10000, sponsorFee, amountsAsStrings = true)
    checkSponsorshipTx(sponsorshipTx)

    checkSponsorshipTx(miner.utx(amountsAsStrings = true).head)
    checkSponsorshipTx(miner.utxById(sponsorshipTx.id))

    val sponsorshipTxHeight           = miner.waitForTransaction(sponsorshipTx.id).height
    val sponsorshipTxBlockLast        = miner.lastBlock(amountsAsStrings = true).transactions.head
    val sponsorshipTxBlockAt          = miner.blockAt(sponsorshipTxHeight, amountsAsStrings = true).transactions.head
    val sponsorshipTxBlockBySignature = miner.blockById(miner.blockAt(sponsorshipTxHeight).id, amountsAsStrings = true).transactions.head
    val sponsorshipTxBlockSeq         = miner.blockSeq(sponsorshipTxHeight, sponsorshipTxHeight, amountsAsStrings = true).head.transactions.head
    checkSponsorshipTx(sponsorshipTxBlockLast)
    checkSponsorshipTx(sponsorshipTxBlockAt)
    checkSponsorshipTx(sponsorshipTxBlockBySignature)
    checkSponsorshipTx(sponsorshipTxBlockSeq)

    val sponsorshipTxInfo = miner.transactionInfo[TransactionInfo](sponsorshipTx.id)
    sponsorshipTxInfo.minSponsoredAssetFee shouldBe Some(10000)
    sponsorshipTxInfo.fee shouldBe sponsorFee
  }
  test("amount as string in masstransfer transaction") {
    nodes.waitForHeightArise()
    overflowBlock()

    def checkMassTransferTx(tx: Transaction): Assertion = {
      tx.transfers.get.head.amount shouldBe transferAmount
      tx.totalAmount shouldBe Some(transferAmount)
    }
    val (transfers, massTransferFee) = (List(Transfer(testAddress, transferAmount)), calcMassTransferFee(1))
    val massTransferTx               = miner.massTransfer(firstKeyPair, transfers, massTransferFee, amountsAsStrings = true)
    checkMassTransferTx(massTransferTx)

    checkMassTransferTx(miner.utx(amountsAsStrings = true).head)
    checkMassTransferTx(miner.utxById(massTransferTx.id, amountsAsStrings = true))

    val massTransferTxHeight           = miner.waitForTransaction(massTransferTx.id).height
    val massTransferTxBlockAt          = miner.blockAt(massTransferTxHeight, amountsAsStrings = true).transactions.head
    val massTransferTxBlockBySignature = miner.blockById(miner.blockAt(massTransferTxHeight).id, amountsAsStrings = true).transactions.head
    val massTransferTxBlockSeq         = miner.blockSeq(massTransferTxHeight, massTransferTxHeight, amountsAsStrings = true).head.transactions.head
    checkMassTransferTx(massTransferTxBlockAt)
    checkMassTransferTx(massTransferTxBlockBySignature)
    checkMassTransferTx(massTransferTxBlockSeq)

    val massTransferTxInfo = miner.transactionInfo[TransactionInfo](massTransferTx.id)
    massTransferTxInfo.transfers.get.head.amount shouldBe transferAmount
    massTransferTxInfo.totalAmount shouldBe Some(transferAmount)

    val tx =
      Json.obj(
        "type"            -> CreateAliasTransaction.typeId,
        "sender"          -> firstKeyPair,
        "alias"           -> "alias",
        "fee"             -> 100000,
        "timestamp"       -> System.currentTimeMillis(),
        "version"         -> 1,
        "senderPublicKey" -> Base58.encode(new Array[Byte](32))
      )
    miner.calculateFee(tx, amountsAsStrings = true).feeAmount shouldBe minFee
  }

  test("amount as string in blocks api") {
    nodes.waitForHeightArise()
    val currentHeight    = miner.height
    val reward           = miner.rewardStatus().currentReward
    val blockLast        = miner.lastBlock(amountsAsStrings = true)
    val blockAt          = miner.blockAt(currentHeight, amountsAsStrings = true)
    val blockBySignature = miner.blockById(miner.lastBlock().id, amountsAsStrings = true)
    val blockHeadersAt   = miner.blockHeadersAt(currentHeight, amountsAsStrings = true)
    val blockHeadersLast = miner.lastBlockHeader(amountsAsStrings = true)

    for (block <- Seq(blockLast, blockAt, blockBySignature)) {
      block.reward shouldBe Some(reward)
      block.desiredReward shouldBe Some(-1)
      block.totalFee shouldBe Some(0)
    }

    for (block <- Seq(blockHeadersLast, blockHeadersAt)) {
      block.reward shouldBe Some(reward)
      block.desiredReward shouldBe Some(-1)
      block.totalFee shouldBe 0
    }

    val blockSeq          = miner.blockSeq(currentHeight, currentHeight, amountsAsStrings = true)
    val blockSeqByAddress = miner.blockSeqByAddress(miner.address, currentHeight, currentHeight, amountsAsStrings = true)

    for (blocks <- Seq(blockSeq, blockSeqByAddress)) {
      blocks.head.reward shouldBe Some(reward)
      blocks.head.desiredReward shouldBe Some(-1)
      blocks.head.totalFee shouldBe Some(0)
    }

    val blockHeadersSeq = miner.blockHeadersSeq(currentHeight, currentHeight, amountsAsStrings = true)
    blockHeadersSeq.head.reward shouldBe Some(reward)
    blockHeadersSeq.head.desiredReward shouldBe Some(-1)
    blockHeadersSeq.head.totalFee shouldBe 0
  }

  test("amount as string in rewards api") {
    val currentHeight    = miner.height
    val rewardsAsInteger = miner.rewardStatus()
    val rewards          = miner.rewardStatus(amountsAsStrings = true)
    val rewardsByHeight  = miner.rewardStatus(Some(currentHeight), amountsAsStrings = true)
    rewards.totalWavesAmount shouldBe rewardsAsInteger.totalWavesAmount
    rewards.currentReward shouldBe rewardsAsInteger.currentReward
    rewards.minIncrement shouldBe rewardsAsInteger.minIncrement
    rewardsByHeight.totalWavesAmount shouldBe rewardsAsInteger.totalWavesAmount
    rewardsByHeight.currentReward shouldBe rewardsAsInteger.currentReward
    rewardsByHeight.minIncrement shouldBe rewardsAsInteger.minIncrement
  }

  test("amount as string in consensus api") {
    val firstGeneratingBalance = miner.balanceDetails(testAddress).generating
    miner.generatingBalance(testAddress, amountsAsStrings = true).balance shouldBe firstGeneratingBalance
  }

  test("amount as string in debug api") {
    val firstBalance = miner.balanceDetails(testAddress).available
    val portfolio    = miner.debugPortfoliosFor(testAddress, considerUnspent = false, amountsAsStrings = true)
    portfolio.balance shouldBe firstBalance
    portfolio.lease.in shouldBe 0
    portfolio.lease.out shouldBe 0

    miner.debugBalanceHistory(testAddress, amountsAsStrings = true).head.balance shouldBe firstBalance

    val stateWavesOnHeight = miner.getWithCustomHeader(
      s"/debug/stateWaves/${miner.height}",
      headerValue = "application/json;large-significand-format=string",
      withApiKey = true
    )
    (parseResponse(stateWavesOnHeight) \ testAddress).get shouldBe JsString(firstBalance.toString)
  }

  private def parseResponse(response: Response): JsValue = Json.parse(response.getResponseBody)
}
