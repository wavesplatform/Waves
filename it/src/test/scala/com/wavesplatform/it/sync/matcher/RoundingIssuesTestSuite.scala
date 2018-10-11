package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType
import scala.concurrent.duration._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class RoundingIssuesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(EthId.toString), None, 2)
  bobNode.transfer(bobNode.address, bobAcc.address, defaultAssetQuantity, 100000, Some(BtcId.toString), None, 2)
  nodes.waitForHeightArise()

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = matcherNode.accountBalances(aliceAcc.address)._1
    val bobBalanceBefore   = matcherNode.accountBalances(bobAcc.address)._1

    val counter   = matcherNode.prepareOrder(aliceAcc, wavesUsdPair, OrderType.BUY, 238, 3100000000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobAcc, wavesUsdPair, OrderType.SELL, 235, 425532L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 420169L
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    matcherNode.cancelOrder(aliceAcc, wavesUsdPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)
    val rawExchangeTx = matcherNode.rawTransactionInfo(tx.id)

    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

    val aliceBalanceAfter = matcherNode.accountBalances(aliceAcc.address)._1
    val bobBalanceAfter   = matcherNode.accountBalances(bobAcc.address)._1

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter   = matcherNode.prepareOrder(bobAcc, ethBtcPair, OrderType.BUY, 31887L, 923431000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(aliceAcc, ethBtcPair, OrderType.SELL, 31887L, 223345000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 223344937L
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    withClue("Alice's reserved balance before cancel")(matcherNode.reservedBalance(aliceAcc) shouldBe empty)

    matcherNode.cancelOrder(bobAcc, ethBtcPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)

    withClue("Bob's reserved balance after cancel")(matcherNode.reservedBalance(bobAcc) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = matcherNode.prepareOrder(bobAcc, wavesUsdPair, OrderType.SELL, 60L, 98333333L)
    matcherNode.placeOrder(counter1).message.id

    val counter2   = matcherNode.prepareOrder(bobAcc, wavesUsdPair, OrderType.SELL, 70L, 100000000L)
    val counter2Id = matcherNode.placeOrder(counter2).message.id

    val submitted   = matcherNode.prepareOrder(aliceAcc, wavesUsdPair, OrderType.BUY, 1000L, 100000000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, counter2Id, "PartiallyFilled", Some(2857143L), 1.minute)
    matcherNode.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(99523810L), 1.minute)

    withClue("orderBook check") {
      val ob = matcherNode.orderBook(wavesUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(70L, 97142857L)) // = 100000000 - 2857143
    }
  }

}
